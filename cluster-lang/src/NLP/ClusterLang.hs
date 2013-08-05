{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Language clustering on the basis of an edit distance
-- string metric and the DBSCAN algorithm.


module NLP.ClusterLang
( 
-- * Configuration
  Conf (..)
-- * Clustering
, Clust (..)
, cluster
-- * Input/output
, showClustering
) where


import           Data.Maybe (catMaybes, maybeToList)
import qualified Data.Vector as V
import qualified Data.Text.Lazy as L

import qualified Data.DAWG.Static as D
import qualified NLP.Adict as A

import qualified NLP.ClusterLang.DisjointSet as DS
import qualified NLP.ClusterLang.EquivRel as EQ

------------------------------------------
-- Clustering configuration
------------------------------------------


-- | Clustering configuration.
data Conf = Conf {
    -- | Minimum number of points required to form a cluster.
      minPts    :: Int  
    -- | A parameter used to identify neighbors of an element.
    , eps       :: Double
    -- | A cost function for restricted generalized edit distance.
    , cost      :: A.Cost Char }


------------------------------------------
-- Language clustering
------------------------------------------


-- | Language clustering.
data Clust = Clust
    { dawg  :: D.DAWG Char D.Weight ()
    , eqRel :: EQ.EquivRel }


-- | Cluster the input list w.r.t. the clustering configuration.
-- Configuration may vary depending on the query word.
-- cluster :: (String -> Conf) -> [String] -> [(String, String)]
cluster :: (String -> Conf) -> [String] -> Clust
cluster mkConf xs = Clust
    { dawg  = dawg
    , eqRel = EQ.fromList (DS.toList disjointSet) }
  where
    -- Convert input to weighted DAWG
    dawg = D.weigh (D.fromLang xs)
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


------------------------------------------
-- Input/output
------------------------------------------


showClustering :: Clust -> L.Text
showClustering Clust{..} =
    L.unlines $ map showCls $ EQ.toList eqRel
  where
    showCls cls = L.unwords $ map L.pack $ catMaybes $ map byIndex cls
    byIndex i = D.byIndex i dawg
