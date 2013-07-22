{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (forM_)
import           Data.Maybe (catMaybes, maybeToList)
import           Data.List (sortBy)
import qualified Data.IntDisjointSet as DS
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L
import           Data.Ord (comparing)

import qualified Data.PoliMorf as P
import qualified Data.DAWG.Static as D
import qualified NLP.Adict as A


-- | DBSCAN implementation.
dbscan
    :: Int                  -- ^ Minimum number of points required
                            --   to form a cluster
    -> (Int -> [Int])       -- ^ Eps-neighborhood of a particular element
    -> [Int]                -- ^ Input elements
    -> DS.IntDisjointSet    -- ^ Resulting clustering
dbscan minPts epsNei xs =
    DS.fromList $ concat [ neiRel x | x <- xs ]
  where
    neiRel x = atLeast minPts [(x, y) | y <- epsNei x]
    atLeast k ys = if length ys >= k
        then ys
        else []
    

-- | DBSCAN configuration.
data Conf = Conf {
    -- | Minimum number of points required to form a cluster.
      minPts    :: Int  
    -- | A parameter used to identify neighbors of an element.
    , eps       :: Double
    -- | A cost function for restricted generalized edit distance.
    , cost      :: A.Cost Char }


-- | Cluster the input set w.r.t. the DBSCAN configuration
-- and the given cost function.
-- cluster :: Conf -> A.Cost Char -> [String] -> [(String, [String])]
cluster :: Conf -> [String] -> [(String, String)]
cluster Conf{..} xs = sortBy (comparing fst) $ map swap $ catMaybes
    [ (,) <$> D.byIndex i dawg
          <*> (lookFor i disj >>= flip D.byIndex dawg)
    | i <- [0 .. D.size dawg - 1] ]
  where
    -- Convert input to weighted DAWG
    dawg = D.weigh (D.fromLang xs)
    -- Define function for identifying neighborhood.
    -- The function works on word identifiers.
    epsNei i = catMaybes
        [ D.index (_1 y) dawg
        | x <- maybeToList (D.byIndex i dawg)
        , y <- A.findAll cost eps (V.fromList x) dawg ]
    -- Run DBSCAN on DAWG elements (or rather, their identifiers).
    disj = dbscan minPts epsNei [0 .. D.size dawg - 1]
    -- Look for an equivalent class of the given element.
    lookFor i = fst . DS.lookup i
    -- First element of a triple.
    _1 (x, _, _) = x
    -- Swap pair elements.
    swap (x, y) = (y, x)


main :: IO ()
main = do
    xs <- map (T.unpack . P.form)
        . P.parsePoliMorf <$> L.getContents
    let cfg = Conf {minPts = 3, eps = 1.0, cost = A.costDefault}
    forM_ (cluster cfg xs) $ \(x, y) -> do
        putStr x >> putStr " => " >> putStrLn y
