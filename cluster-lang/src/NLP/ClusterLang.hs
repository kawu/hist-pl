{-# LANGUAGE RecordWildCards #-}


-- | Language clustering on the basis of an edit distance
-- string metric and the DBSCAN algorithm.


module NLP.ClusterLang
( Conf (..)
, cluster
) where


import           Control.Applicative (pure, (<$>), (<*>))
-- import           Control.Monad (forM_)
import           Data.Maybe (catMaybes, maybeToList)
import           Data.List (sort)
import qualified Data.Vector as V
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy.IO as L

-- import qualified Data.PoliMorf as P
import qualified Data.DAWG.Static as D
import qualified NLP.Adict as A

import qualified NLP.ClusterLang.DisjointSet as DS
import qualified NLP.ClusterLang.DBSCAN as DB


-- | Clustering configuration.
data Conf = Conf {
    -- | Minimum number of points required to form a cluster.
      minPts    :: Int  
    -- | A parameter used to identify neighbors of an element.
    , eps       :: Double
    -- | A cost function for restricted generalized edit distance.
    , cost      :: A.Cost Char }


-- | Cluster the input set w.r.t. the DBSCAN configuration
-- and the given cost function.
cluster :: Conf -> [String] -> [(String, String)]
cluster Conf{..} xs
    = catMaybes $ map (\(i, j) ->
        (,) <$> byID i <*> byID j)
    $ sort $ catMaybes
        [ (,) <$> eqCls i <*> pure i
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
    dbCfg = DB.Conf { DB.minPts = minPts, DB.epsNei = epsNei }
    disj  = DB.dbscan dbCfg (D.size dawg)
    -- Look for an equivalent class element of the given element.
    eqCls = flip DS.lookup disj
    -- Retrive word given its DAWG index.
    byID = flip D.byIndex dawg
    -- First element of a triple.
    _1 (x, _, _) = x


-- main :: IO ()
-- main = do
--     xs <- map (T.unpack . P.form)
--         . P.parsePoliMorf <$> L.getContents
--     let cfg = Conf {minPts = 3, eps = 1.0, cost = A.costDefault}
--     forM_ (cluster cfg xs) $ \(x, y) -> do
--         putStr x >> putStr " => " >> putStrLn y
