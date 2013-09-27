module NLP.HistPL.Similar.Cost
( costSpecial
, costPosMod
) where


import           Data.Char
import qualified Data.Map as M

import qualified NLP.Adict as A


-- | Weight (cost) of changing (toUpper x) character to (toLower x)
-- character and vice versa.
lowerEqWeight :: Double
lowerEqWeight = 0.5


-- | A cost with special rules.
costSpecial :: Int -> A.Cost Char
costSpecial n =
    
    A.Cost
        { A.insert  = \k x   -> posMod k * insert x
        , A.delete  = \k x   -> posMod k * delete x
        , A.subst   = \k x y -> posMod k * subst x y }

  where
    
    -- Insert
    insert = const 1

    -- Delete
    -- TODO: We probably don't need this right now.
    delete x | isPunctuation x = 0.5
             | otherwise       = 1

    -- Substitute
    subst x y
        | x == y    = 0
        | x ~== y   = lowerEqWeight
        | otherwise = case M.lookup (x, y) substMatrix of
            Just w  -> w
            Nothing -> 1.0
        where a ~== b = toUpper a == toUpper b


    -- Position modifier.
    posMod k
        | k <= n_2  = 1
        | otherwise = (n - k + 1) ./. (n - n_2 + 1)
    x ./. y = fromIntegral x / fromIntegral y
    n_2 = (n + 1) `div` 2


-- | Substitution matrix.
substMatrix :: M.Map (Char, Char) A.Weight
substMatrix = M.fromList $ concatMap (uncurry mkGroup)
    [ ("aą", 0.5)
    , ("eę", 0.5)
    , ("oó", 0.5)
    , ("cć", 0.5)
    , ("lł", 0.5)
    , ("nń", 0.5)
    , ("sś", 0.5)
    , ("zźż", 0.5)
    , ("yij", 0.75)
    , ("sz", 0.75)
    , ("kc", 0.87) ]
  where
    mkGroup xs w = concat
        [ [ ((x, y), w)
          , ((toUpper x, toUpper y), w)
          , ((toUpper x, y), w + lowerEqWeight)
          , ((x, toUpper y), w + lowerEqWeight) ]
        | x <- xs , y <- xs , x /= y ]


-- | A standrad cost with additional position modifier.
costPosMod :: Int -> A.Cost Char
costPosMod n = A.Cost
    { A.insert  = \k x   -> posMod k * insert x
    , A.delete  = \k x   -> posMod k * delete x
    , A.subst   = \k x y -> posMod k * subst x y }
  where
    insert = const 1
    delete = const 1
    subst x y
        | x == y    = 0
        | otherwise = 1
    posMod k
        | k <= n_2  = 1
        | otherwise = (n - k + 1) ./. (n - n_2 + 1)
    x ./. y = fromIntegral x / fromIntegral y
    n_2 = (n + 1) `div` 2
