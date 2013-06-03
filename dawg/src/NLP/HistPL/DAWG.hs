{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


-- | A `DS.DAWG`-based dictionary with additional information
-- assigned to lexical entries and word forms.


module NLP.HistPL.DAWG
(
-- * Rule
  Rule (..)
, apply
, between

-- * Entry
, Lex (..)
, Key (..)
, Val (..)

-- * Entry set
, LexSet
, mkLexSet
, unLexSet
, Node
, decode


-- * DAWG
, DAWG

-- ** Initialization
, DAWG'Init
, DM.empty
, insert
, DS.freeze

-- ** Query
, lookup
, submap

-- ** Weight
, DS.Weight
, DS.weigh
, DS.size
, index
, byIndex

-- ** Conversion
, fromList
, toList
, entries
, revDAWG
) where


import           Prelude hiding (lookup)
import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (first)
import           Data.Binary (Binary, get, put)
import           Data.Text.Binary ()
import           Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.DAWG.Static as DS
import qualified Data.DAWG.Dynamic as DM


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
    -- | A path of the entry, i.e. DAWG key.
      path  :: T.Text
    -- | Unique identifier among entries with the same `path`.
    , uid   :: i }
    deriving (Show, Eq, Ord)


-- | A value of the entry.
data Val a w b = Val {
    -- | Additional information assigned to the entry.
      info  :: a
    -- | A map of forms with additional info of type @b@ assigned.
    -- Invariant: in case of a reverse dictionary (from word forms
    -- to base forms) this map should contain exactly one element
    -- (a base form and a corresonding information).
    , forms :: M.Map w b }
    deriving (Show, Eq, Ord)


instance (Ord w, Binary a, Binary w, Binary b) => Binary (Val a w b) where
    put Val{..} = put info >> put forms
    get = Val <$> get <*> get


-- | A dictionary entry consists of a `Key` and a `Val`ue.
data Lex i a b = Lex {
    -- | Entry key.
      lexKey :: Key i
    -- | Entry value.
    , lexVal :: Val a T.Text b }
    deriving (Show, Eq, Ord)


------------------------------------------------------------------------
-- Set of entries 
------------------------------------------------------------------------


-- | A set of dictionary entries.
type LexSet i a b = M.Map (Key i) (Val a T.Text b)


-- | Make lexical set from a list of entries.
mkLexSet :: Ord i => [Lex i a b] -> LexSet i a b
mkLexSet = M.fromList . map ((,) <$> lexKey <*> lexVal)


-- | List lexical entries.
unLexSet :: LexSet i a b -> [Lex i a b]
unLexSet = map (uncurry Lex) . M.toList


-- | Map function over entry word forms.
mapW :: Ord w' => (w -> w') -> Val a w b -> Val a w' b
mapW f v =
    let g = M.fromList . map (first f) . M.toList
    in  v { forms = g (forms v) }


-- | Actual values stored in automaton states contain
-- all entry information but `path`.
type Node i a b = M.Map i (Val a Rule b)


-- | Decode dictionary value given `path`.
decode :: Ord i => T.Text -> Node i a b -> LexSet i a b
decode x n = M.fromList
    [ (Key x i, mapW (flip apply x) val)
    | (i, val) <- M.toList n ]


-- | Transform entry into a list.
toListE :: Lex i a b -> [(T.Text, i, a, T.Text, b)]
toListE (Lex Key{..} Val{..}) =
    [ (path, uid, info, form, y)
    | (form, y) <- M.assocs forms ]


------------------------------------------------------------------------
-- DAWG 
------------------------------------------------------------------------


-- | A dictionary parametrized over ID @i@, with info @a@ for every
-- (key, i) pair and info @b@ for every (key, i, apply rule key) triple.
type DAWG i a b = DS.DAWG Char DS.Weight (Node i a b)


------------------------------------------------------------------------
-- Initialization 
------------------------------------------------------------------------


-- | A `DAWG` initialization structure (a dynamic DAWG).
type DAWG'Init i a b = DM.DAWG Char (Node i a b)


-- | Insert a (key, ID, entry info, form, entry\/form info) into a
-- `DAWG'Init` structure.
insert
    :: (Ord i, Ord a, Ord b)
    => (T.Text, i, a, T.Text, b)
    -> DAWG'Init i a b
    -> DAWG'Init i a b
insert (x, i, a, y, b) = DM.insertWith union
    (T.unpack x)
    (M.singleton i (Val a (M.singleton (between x y) b)))
  where
    union = M.unionWith $ both const M.union
    both f g (Val x0 y0) (Val x1 y1) = Val (f x0 x1) (g y0 y1)


------------------------------------------------------------------------
-- Query 
------------------------------------------------------------------------


-- | Lookup the key in the dictionary.
lookup :: Ord i => T.Text -> DAWG i a b -> LexSet i a b
lookup x dict = decode x $ case DS.lookup (T.unpack x) dict of
    Just m  -> m
    Nothing -> M.empty


-- | Return the sub-dictionary containing all keys beginning with a prefix.
submap :: Ord i => T.Text -> DAWG i a b -> Maybe (DAWG i a b)
submap x dict = DS.submap (T.unpack x) dict


-- | Position in a set of all dictionary entries with respect
-- to the lexicographic order.
index :: T.Text -> DAWG i a b -> Maybe Int
index x = DS.index (T.unpack x)


-- | Find dictionary entry given its index with respect to the
-- lexicographic order.
byIndex :: Int -> DAWG i a b -> Maybe T.Text
byIndex ix = fmap T.pack . DS.byIndex ix


------------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------------


-- | List dictionary lexical entries.
entries :: Ord i => DAWG i a b -> [Lex i a b]
entries = concatMap f . DS.assocs where
    f (key, val) = unLexSet $ decode (T.pack key) val


-- | Make dictionary from a list of (key, ID, entry info, form,
-- entry\/form info) tuples.
fromList :: (Ord i, Ord a, Ord b) => [(T.Text, i, a, T.Text, b)] -> DAWG i a b
fromList = DS.weigh . DS.freeze . foldl' (flip insert) DM.empty


-- | Transform dictionary back into the list of (key, ID, key\/ID info, elem,
-- key\/ID\/elem info) tuples.
toList :: (Ord i, Ord a, Ord b) => DAWG i a b -> [(T.Text, i, a, T.Text, b)]
toList = concatMap toListE . entries


-- | Reverse the dictionary.
revDAWG :: (Ord i, Ord a, Ord b) => DAWG i a b -> DAWG i a b
revDAWG = 
    let swap (base, i, x, form, y) = (form, i, x, base, y)
    in  fromList . map swap . toList
