{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


-- | A `D.DAWG`-based dictionary.


module NLP.HistPL.Dict
(
-- * Rule
  Rule (..)
, apply
, between

-- * Basic types
, Key
, Word

-- * Dictionary
, Dict
-- ** Entry
, Entry
, mapW
, encode
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
-- Basic types
------------------------------------------------------------------------


-- | Key form of an entry (e.g. lemma).
type Key = T.Text


-- | Word form.
type Word = T.Text


------------------------------------------------------------------------
-- Entry componenets (key and value)
------------------------------------------------------------------------


-- | A dictionary entry.  It contains all information about the
-- entry except the key form, which constitutes a key
-- in a `D.DAWG` dictionary.
type Entry i a w b = M.Map i (a, M.Map w b)


-- | Map function over entry forms. 
mapW :: (Ord i, Ord w') => (w -> w') -> Entry i a w b -> Entry i a w' b
mapW f =
    M.fromList . map (second2 mapForms) . M.toList
  where
    second2 = second . second
    mapForms = M.fromList . map (first f) . M.toList


-- | Encode dictionary entry.
encode :: Ord i => T.Text -> Entry i a Word b -> Entry i a Rule b
encode = mapW . between


-- | Decode dictionary entry.
decode :: Ord i => T.Text -> Entry i a Rule b -> Entry i a Word b
decode = mapW . flip apply


-- | Transform entry into a list.
listEntry :: Key -> Entry i a w b -> [(Key, i, a, w, b)]
listEntry key entry =
    [ (key, uid, info, word, y)
    | (uid, (info, forms)) <- M.assocs entry
    , (word, y) <- M.assocs forms ]


------------------------------------------------------------------------


-- | A dictionary parametrized over ID @i@, with info @a@ for every
-- (key, i) pair and info @b@ for every (key, i, apply rule key) triple.
type Dict i a b = D.DAWG Char () (Entry i a Rule b)


-- | Lookup the key in the dictionary.
lookup :: Ord i => T.Text -> Dict i a b -> Entry i a Word b
lookup key dict = decode key $ case D.lookup (T.unpack key) dict of
    Just m  -> m
    Nothing -> M.empty


-- | List dictionary lexical entries.
entries :: Ord i => Dict i a b -> [(Key, Entry i a Word b)]
entries = map f . D.assocs where
    f (key, entry) =
        let key' = T.pack key
        in  (key', decode key' entry)


-- | Make dictionary from a list of (key, ID, entry info, word,
-- entry\/word info) tuples.
fromList :: (Ord i, Ord a, Ord b) => [(Key, i, a, Word, b)] -> Dict i a b
fromList xs = D.fromListWith union $
    [ ( T.unpack x
      , M.singleton i (a, M.singleton (between x y) b) )
    | (x, i, a, y, b) <- xs ]
  where
    union = M.unionWith $ both const M.union
    both f g (x, y) (x', y') = (f x x', g y y')


-- | Transform dictionary back into the list of (key, ID, key\/ID info, elem,
-- key\/ID\/elem info) tuples.
toList :: (Ord i, Ord a, Ord b) => Dict i a b -> [(Key, i, a, Word, b)]
toList = concatMap (uncurry listEntry) . entries


-- | Reverse the dictionary.
revDict :: (Ord i, Ord a, Ord b) => Dict i a b -> Dict i a b
revDict = 
    let swap (base, i, x, form, y) = (form, i, x, base, y)
    in  fromList . map swap . toList
