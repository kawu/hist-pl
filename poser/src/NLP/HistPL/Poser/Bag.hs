module NLP.HistPL.Poser.Bag
( Bag
, empty
, fromList
, toList
, mostCommon
, union
, unions
) where

import qualified Data.Map as M

type Bag a = M.Map a Double

empty :: Ord a => Bag a
empty = fromList []

fromList :: Ord a => [(a, Double)] -> Bag a
fromList = M.fromListWith (+)

toList :: Bag a -> [(a, Double)]
toList = M.toList

mostCommon :: Bag a -> [a]
mostCommon bag =
    let mv = maximum [k | (_, k) <- M.assocs bag]
    in  [x | (x, k) <- M.assocs bag, k == mv]

union :: Ord a => Bag a -> Bag a -> Bag a
union = M.unionWith (+)

unions :: Ord a => [Bag a] -> Bag a
unions = M.unionsWith (+)
