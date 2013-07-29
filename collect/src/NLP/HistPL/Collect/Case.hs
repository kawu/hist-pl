module NLP.HistPL.Collect.Case
( detCase
) where


import qualified Data.Char as C
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T


-- | Number of occurences with different cases.
data Coc = Coc
    { _low  :: {-# UNPACK #-} !Int
    -- ^ All character lower-cased.
    , _up   :: {-# UNPACK #-} !Int
    -- ^ Otherwise.
    } deriving (Show, Eq, Ord)


-- | Add two `Coc` elements.
(.+.) :: Coc -> Coc -> Coc
Coc x y .+. Coc x' y' = Coc (x+x') (y+y')


-- | Determine, which words should be upper-cased, and which should
-- be lower-cased.  The first argument represents the minimum frequency,
-- with which the given word have to occur in upper-cased form to be
-- deemed as actually upper-cased (e.g. named entity).
--
-- WARNING: the resulting function will lower-case the words, which
-- have not been encountered in the input list.
detCase :: Double -> [T.Text] -> (T.Text -> T.Text)
detCase k xs = \x ->
    if T.toLower x `S.member` upperSet
        then x else T.toLower x
  where
    upperSet = S.fromList
        [x | (x, coc) <- M.toList cocMap, cocUpper coc]
    cocUpper (Coc l u) =
        (fromIntegral u / fromIntegral (l + u)) > k
    cocMap = M.unionsWith (.+.) $ map mkItem $ xs
    mkItem x
        | T.any C.isUpper x = M.singleton xl $ Coc 0 1
        | otherwise         = M.singleton xl $ Coc 1 0
        where xl = T.toLower x
