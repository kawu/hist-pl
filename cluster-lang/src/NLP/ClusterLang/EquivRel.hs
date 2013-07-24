{-# LANGUAGE RecordWildCards #-}


-- | Another representation of an equivalence relation (clustering)
-- for integer {0..k} ranges.


module NLP.ClusterLang.EquivRel
(
-- * Types
  ClsID
, EquivRel

-- * Build
, fromPairs

-- * Query
, elemNum
, clsNum
, clsList
-- ** Cluster-wise
, clsItems
, repr
) where


import qualified Control.Monad.ST as ST
import           Control.Monad (forM_)
import           Data.Function (on)
import           Data.List (sort, groupBy)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM


-----------------------------------------
-- Types
-----------------------------------------


-- | Identifier of a class.
type ClsID = Int


-- | A clustering (or an equivalence relation).
data EquivRel = EquivRel {
    -- | Vector of clusters.
      clVect :: V.Vector (U.Vector Int)
    -- | For assigning clusters to individual elements.
    , clFor  :: U.Vector ClsID }


-----------------------------------------
-- Build
-----------------------------------------


-- | Construct clustering from a list of relation pairs.
-- Its not lazy, it will consume all the list at once
-- (since it needs to perform sorting, among others).
fromPairs :: [(Int, Int)] -> EquivRel
fromPairs =
    let nub = S.toList . S.fromList
        asList (x, y) = [x, y]
        mergeGrp = nub . concatMap asList
    in  fromList . map mergeGrp . groupBy ((==) `on` fst) . sort


-- | Construct clustering from a list of clusters.  The property,
-- that individual clusters are mutually disjoint, is not checked.
-- Neither is the property, that elements cover the relevant domain.
-- If any of the clusters is empty, the function will fail with error.
fromList :: [[Int]] -> EquivRel
fromList xs = EquivRel
    { clVect = clVect
    , clFor  = clFor }
  where
    clVect = V.fromList (map U.fromList xs)
    mx = (+1) $ maximum $ map U.maximum $ V.toList clVect
    clFor = ST.runST $ do
        clForM <- UM.replicate mx 0
        forM_ (zip (V.toList clVect) [0..]) $ \(cls, clsID) -> do
            forM_ (U.toList cls) $ \x -> do
                UM.write clForM x clsID
        U.unsafeFreeze clForM


-----------------------------------------
-- Query
-----------------------------------------


-- | Number of distint elements in the clustering.
elemNum :: EquivRel -> Int
elemNum = U.length . clFor


-- | Number of clusters.
clsNum :: EquivRel -> Int
clsNum = V.length . clVect


-- | List of relation equivalence classes (clusters).
clsList :: EquivRel -> [ClsID]
clsList eqRel = [0 .. clsNum eqRel - 1]


-----------------------------------------
-- Cluster-wise
-----------------------------------------


-- | List all elements of a class.
clsItems :: EquivRel -> ClsID -> [Int]
clsItems EquivRel{..} = U.toList . (clVect V.!)


-- | Representant of a class.
repr :: EquivRel -> ClsID -> Int
repr EquivRel{..} = U.head . (clVect V.!)
