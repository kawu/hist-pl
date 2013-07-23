{-# LANGUAGE RecordWildCards #-}


module NLP.ClusterLang.DBSCAN
( Conf (..)
, dbscan
) where


import qualified NLP.ClusterLang.DisjointSet as DS


-- | DBSCAN configuration.
data Conf = Conf {
    -- | Minimum number of points required to form a cluster.
      minPts    :: Int
    -- | Eps-neighborhood of a particular element.
    , epsNei    :: Int -> [Int] }


-- | DBSCAN clustering algorithm on 'Int's.
dbscan
    :: Conf                 -- ^ Configuration
    -> Int                  -- ^ Input elements of the {0..k-1} form
    -> DS.DisjSet           -- ^ Resulting clustering
dbscan Conf{..} k =
    DS.fromList k $ concat [ neiRel x | x <- [0..k-1] ]
  where
    neiRel x = atLeast minPts [(x, y) | y <- epsNei x]
    atLeast n ys = if length ys >= n
        then ys
        else []
