{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


-- | A `D.DAWG`-based dictionary.


module NLP.HistPL.Dict
(
-- * Rule
  Rule (..)
, apply
, between

-- * Dictionary
, Dict
-- ** Entry
, Lex (..)
, Key (..)
, Val
-- , encode
, decode
-- ** Query
, lookup
-- ** Conversion
, fromList
, toList
, entries
, revDict
) where


import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, second)
import Data.Binary (Binary, get, put)
import Data.Text.Binary ()
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.DAWG.Static as D


------------------------------------------------------------------------
-- Rule
------------------------------------------------------------------------


-- | A rule for translating a form into another form.
data Rule = Rule {
    -- | Number of characters to cut from the end of the form.
      cut       :: !Int
    -- | A suffix to paste.
    , suffix    :: !T.Text
    } deriving (Show, Eq, Ord)


instance Binary Rule where
    put Rule{..} = put cut >> put suffix
    get = Rule <$> get <*> get


-- | Apply the rule.
apply :: Rule -> T.Text -> T.Text
apply r x = T.take (T.length x - cut r) x `T.append` suffix r


-- | Determine a rule which translates between two strings.
between :: T.Text -> T.Text -> Rule
between source dest =
    let k = lcp source dest
    in  Rule (T.length source - k) (T.drop k dest)
  where
    lcp a b = case T.commonPrefixes a b of
        Just (c, _, _)  -> T.length c
        Nothing         -> 0


------------------------------------------------------------------------
-- Entry componenets (key and value)
------------------------------------------------------------------------


-- | A key of a dictionary entry.
data Key i = Key {
    -- | A main, orthographic form of the entry.
      orth  :: T.Text
    -- | Unique identifier among entries with the same `orth`.
    , uid   :: i }
    deriving (Show, Eq, Ord)


-- | A value of the entry.
type Val a w b = (a, M.Map w b)


-- | A dictionary entry consists of a `Key` and a `Val`ue.
data Lex i a b = Lex {
    -- | Entry key.
      key :: Key i
    -- | Entry value.
    , val :: Val a T.Text b }
    deriving (Show, Eq, Ord)


-- | A set of dictionary entries.
type LexSet i a b = M.Map (Key i) (Val a T.Text b)


-- | Actual values stored in automaton states contain
-- all information but `orth`.
type Node i a b = M.Map i (a, M.Map Rule b)


-- | Map function over entry word forms.
mapW :: Ord w' => (w -> w') -> Val a w b -> Val a w' b
mapW f = second $ M.fromList . map (first f) . M.toList


-- | Encode dictionary value given `orth`.


-- | Decode dictionary value given `orth`.
decode :: Ord i => T.Text -> Node i a b -> LexSet i a b
decode x n = M.fromList
    [ (Key x i, mapW (flip apply x) val)
    | (i, val) <- M.toList n ]


-- -- | Encode dictionary value.
-- encode :: Ord i => T.Text -> Val i a T.Text b -> Val i a Rule b
-- encode = mapW . between


-- | Transform entry into a list.
toListE :: Lex i a b -> [(T.Text, i, a, T.Text, b)]
toListE (Lex Key{..} (x, forms)) =
    [ (orth, uid, x, form, y)
    | (form, y) <- M.assocs forms ]


------------------------------------------------------------------------


-- | A dictionary parametrized over ID @i@, with info @a@ for every
-- (key, i) pair and info @b@ for every (key, i, apply rule key) triple.
type Dict i a b = D.DAWG Char () (Node i a b)


-- | Lookup the key in the dictionary.
lookup :: Ord i => T.Text -> Dict i a b -> LexSet i a b
lookup x dict = decode x $ case D.lookup (T.unpack x) dict of
    Just m  -> m
    Nothing -> M.empty


-- | List dictionary lexical entries.
entries :: Ord i => Dict i a b -> [Lex i a b]
entries = concatMap f . D.assocs where
    f (key, val) = listIt $ decode (T.pack key) val
    listIt = map (uncurry Lex) . M.toList


-- | Make dictionary from a list of (key, ID, entry info, form,
-- entry\/form info) tuples.
fromList :: (Ord i, Ord a, Ord b) => [(T.Text, i, a, T.Text, b)] -> Dict i a b
fromList xs = D.fromListWith union $
    [ ( T.unpack x
      , M.singleton i (a, M.singleton (between x y) b) )
    | (x, i, a, y, b) <- xs ]
  where
    union = M.unionWith $ both const M.union
    both f g (x, y) (x', y') = (f x x', g y y')


-- | Transform dictionary back into the list of (key, ID, key\/ID info, elem,
-- key\/ID\/elem info) tuples.
toList :: (Ord i, Ord a, Ord b) => Dict i a b -> [(T.Text, i, a, T.Text, b)]
toList = concatMap toListE . entries


-- | Reverse the dictionary.
revDict :: (Ord i, Ord a, Ord b) => Dict i a b -> Dict i a b
revDict = 
    let swap (base, i, x, form, y) = (form, i, x, base, y)
    in  fromList . map swap . toList
