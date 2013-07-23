{-# LANGUAGE RecordWildCards #-}


-- | Language clustering on the basis of an edit distance
-- string metric and the DBSCAN algorithm.


module NLP.ClusterLang
( Conf (..)
, cluster
) where


import           Control.Applicative (pure, (<$>), (<*>))
import           Data.Maybe (catMaybes, maybeToList)
import           Data.List (sort)
import qualified Data.Vector as V

import qualified Data.DAWG.Static as D
import qualified NLP.Adict as A

import qualified NLP.ClusterLang.DisjointSet as DS
-- import qualified NLP.ClusterLang.DBSCAN as DB


-- | Clustering configuration.
data Conf = Conf {
    -- | Minimum number of points required to form a cluster.
      minPts    :: Int  
    -- | A parameter used to identify neighbors of an element.
    , eps       :: Double
    -- | A cost function for restricted generalized edit distance.
    , cost      :: A.Cost Char }


-- | Cluster the input list w.r.t. the clustering configuration.
-- Configuration may vary depending on the query word.
cluster :: (String -> Conf) -> [String] -> [(String, String)]
cluster mkConf xs
    = catMaybes $ map (\(i, j) ->
        (,) <$> byID i <*> byID j)
    $ sort $ catMaybes
        [ (,) <$> eqCls i <*> pure i
        | i <- [0 .. D.size dawg - 1] ]
  where
    -- Convert input to weighted DAWG
    dawg = D.weigh (D.fromLang xs)
    -- Retrieve word given its DAWG index.
    byID = flip D.byIndex dawg

    -- Define function for identifying neighborhood.
    -- The function works on word identifiers.
    epsNei i = catMaybes
        [ D.index (let (a, _, _) = y in a) dawg
        | x <- maybeToList (D.byIndex i dawg)
        , let Conf{..} = mkConf x
        , y <- atLeast minPts $
            A.findAll cost eps (V.fromList x) dawg ]
    atLeast n ys = if length ys >= n then ys else []

    -- Compute the disjoint-set forest on the basis of the epsNei function.
    -- If the configuration `Conf` is constant, it works like DBSCAN.
    disjointSet = let k = D.size dawg in DS.fromList k
        [(i, j) | i <- [0..k-1], j <- epsNei i]
    -- Look for an equivalent class element of the given element.
    eqCls = flip DS.lookup disjointSet
