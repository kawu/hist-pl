{-# LANGUAGE RecordWildCards #-}


-- | Another representation of an equivalence relation (clustering)
-- for integer {0..k} ranges.


module NLP.ClusterLang.EquivRel
(
-- * Types
  ClsID
, EquivRel

-- * Build
, fromList

-- * Query
, elemNum
, clsNum
, toList
-- ** Cluster-wise
, clsItems
, repr

-- * Comparison
, sim
) where


import qualified Control.Monad.ST as ST
import           Control.Monad (forM_)
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


-- | Construct clustering from a list of clusters.  The property
-- that individual clusters are mutually disjoint is not checked.
-- Neither is the property that elements cover the relevant domain.
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
toList :: EquivRel -> [[Int]]
toList = map U.toList . V.toList . clVect


-- -- | List of relation equivalence classes (clusters).
-- clsList :: EquivRel -> [ClsID]
-- clsList eqRel = [0 .. clsNum eqRel - 1]


-- | Determine cluster of a given element.
clsOf :: EquivRel -> Int -> ClsID
clsOf eq i = clFor eq U.! i
{-# INLINE clsOf #-}


-----------------------------------------
-- Cluster-wise
-----------------------------------------


-- | List all elements of a class.
clsItems :: EquivRel -> ClsID -> [Int]
clsItems EquivRel{..} = U.toList . (clVect V.!)


-- | Representant of a class.
repr :: EquivRel -> ClsID -> Int
repr EquivRel{..} = U.head . (clVect V.!)


-----------------------------------------
-- Comparison
-----------------------------------------


-- | Similarity measure between both relations.
sim
    :: EquivRel     -- ^ Source cluster
    -> EquivRel     -- ^ Reference relation
    -> Int
sim xr yr = sum
    [ correspNum cls yr
    | cls <- toList xr ]


-- Compute the number of reference clusters corresponding to a
-- cluster from the source relation.
correspNum
    :: [Int]        -- ^ Source cluster
    -> EquivRel     -- ^ Reference relation
    -> Int
correspNum xs eqRel = S.size $ S.fromList $ map (clsOf eqRel) xs
