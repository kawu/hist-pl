{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module NLP.Polh.Fusion
(
-- * Rules
  Rule (..)
, apply
, between

-- * Basic types
, UID
, POS
, Word
, Base
, IsBase

-- * Dictionaries
, Dict
, BaseDict
, FormDict
, mkDict
, unDict
, lookup
-- ** Bilateral
, Bila (..)
, mkBila

-- * Historical dictionary
, Hist
, mkHist
, HLex (..)
, enumHist

-- * Contemporary dictionary
, fromPoli
, LexID
, LexSet

-- * Corresponding lexemes
, Corresp
, buildCorresp
-- ** Components
, Core
, Filter
, Choice
-- ** Implementations
, byForms
, posFilter
, sumChoice

-- * Fusion
, Code (..)
, extend
, fuse
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (maybeToList)
import Data.Binary (Binary, get, put)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.PoliMorf as P
import qualified Data.DAWG.Static as D

import qualified NLP.Polh.Types as H
import qualified NLP.Polh.Binary as H

-- | A rule for translating a form into another one.
data Rule = Rule {
    -- | Number of characters to cut from the end of the form.
      cut       :: !Int
    -- | A suffix to paste.
    , suffix    :: !T.Text }
    deriving (Show, Eq, Ord)

instance Binary Rule where
    put Rule{..} = put cut >> put suffix
    get = Rule <$> get <*> get

-- | Apply the rule.
apply :: Rule -> T.Text -> T.Text
apply r x = T.take (T.length x - cut r) x `T.append` suffix r

-- | Make a rule to translate between two strings.
between :: T.Text -> T.Text -> Rule
between source dest =
    let k = lcp source dest
    in  Rule (T.length source - k) (T.drop k dest)
  where
    lcp a b = case T.commonPrefixes a b of
        Just (c, _, _)  -> T.length c
        Nothing         -> 0

------------------------------------------------------------------------

-- | Unique ID in historical dictionary.
type UID = Int

-- | Part of speech.
type POS = T.Text

-- | Base form.
type Base = T.Text

-- | Word form.
type Word = T.Text

-- | Is it a base form?
type IsBase = Bool

-- | One-way dictionary parametrized over ID @i@, with info @a@ for every
-- (key, i) pair and info @b@ for every (key, i, apply rule key) triple.
type Dict i a b = D.DAWG Char () (M.Map i (a, M.Map Rule b))

-- Dictionary keys include base forms and rules transform base forms to
-- their corresponding word forms.  Info @a@ is assigned to every lexeme
-- and info @b@ to every word form.
type BaseDict i a b = Dict i a b

-- Dictionary keys include word forms and rules transform word forms to
-- their corresponding base forms.  Info @a@ is assigned to every word
-- form.
type FormDict i a = Dict i () a

-- | Lookup the key in the dictionary.
lookup :: Ord i => T.Text -> Dict i a b -> M.Map i (a, M.Map Word b)
lookup x'Text dict = M.fromList
    [ (i, (a, M.fromList
        [ (apply rule x'Text, b)
        | (rule, b) <- M.assocs ruleMap ]))
    | m <- lookup' x'Text dict
    , (i, (a, ruleMap)) <- M.assocs m ]
  where
    lookup' x = maybeToList . D.lookup (T.unpack x)

-- | Make dictionary from a list of (key, ID, key/ID info, elem,
-- key/ID/elem info) tuples.
mkDict :: (Ord i, Ord a, Ord b) => [(T.Text, i, a, T.Text, b)] -> Dict i a b
mkDict xs = D.fromListWith union $
    [ ( T.unpack x
      , M.singleton i
        (a, M.singleton (between x y) b) )
    | (x, i, a, y, b) <- xs ]
  where
    union = M.unionWith $ both const M.union
    both f g (x, y) (x', y') = (f x x', g y y')

-- | Transform dictionary back into the list of (key, ID, key/ID info, elem,
-- key/ID/elem info) tuples.
unDict :: (Ord i, Ord a, Ord b) => Dict i a b -> [(T.Text, i, a, T.Text, b)]
unDict dict =
    [ (key, i, a, apply rule key, b)
    | (key'String, lexSet) <- D.assocs dict
    , let key = T.pack key'String
    , (i, (a, ruleMap)) <- M.assocs lexSet
    , (rule, b) <- M.assocs ruleMap ]

-- | Bilateral dictionary.
data Bila i a b = Bila
    { baseDict  :: BaseDict i a b
    , formDict  :: FormDict i b }
    deriving (Show, Eq, Ord)

instance (Ord i, Binary i, Binary a, Binary b) => Binary (Bila i a b) where
    put Bila{..} = put baseDict >> put formDict
    get = Bila <$> get <*> get

-- | Make bilateral dictionary from a list of (base form, ID, additional
-- lexeme info, word form, additional word form info) tuples.
mkBila :: (Ord i, Ord a, Ord b) => [(T.Text, i, a, T.Text, b)] -> Bila i a b
mkBila xs = Bila
    { baseDict  = baseDict'
    , formDict  = formDict' }
  where
    baseDict'   = mkDict xs
    formDict'   = mkDict . map swap . unDict $ baseDict'
    swap (base, i, _, form, y) = (form, i, (), base, y)

-- | Make bilateral dictionary from PoliMorf.
fromPoli :: [P.Entry] -> Bila POS () ()
fromPoli = mkBila . map ((,,(),,()) <$> P.base <*> P.pos <*> P.form)

-- | Historical dictionary.
type Hist = Dict UID (S.Set POS) IsBase

-- | Construct historical dictionary.
mkHist :: [H.BinEntry] -> Hist
mkHist xs = mkDict
    [ ( H.keyForm key
      , H.keyUid key
      , S.fromList (H.pos entry)
      , form
      , isBase )
    | binEntry <- xs
    , let key = H.binKey binEntry
    , let entry = H.entry binEntry
    , (form, isBase) <-
        map (,True) (lemmas entry) ++
        map (,False) (forms entry)
    , oneWord form ]
  where
    lemmas = H.text . H.lemma
    forms  = concatMap H.text . H.forms

-- | Is it a one-word text?
oneWord :: T.Text -> Bool
oneWord = (==1) . length . T.words

-- | Entry from historical dictionary.
data HLex a = HLex
    { hKey      :: T.Text
    , hUID      :: UID
    , hPOSs     :: S.Set POS
    , hWords    :: M.Map Word a }
    deriving (Show, Eq, Ord)

-- | List all lexical entries from historical dictionary.
enumHist :: Hist -> [HLex IsBase]
enumHist hist =
    [ HLex key'Text uid poss $ M.fromList
        [ (apply rule key'Text, isBase)
        | (rule, isBase)    <- M.assocs rules ]
    | (key, idMap)          <- D.assocs hist
    , let key'Text          =  T.pack key
    , (uid, (poss, rules))  <- M.assocs idMap ]

-- | Lexeme ID in contemporary dictionary (e.g. PoliMorf).
type LexID = (Base, POS)

-- | Set of lexemes.
type LexSet = M.Map LexID (S.Set Word)

-- | Type of a function which determines lexemes from a bilateral
-- dictionary corresponing to a given historical lexeme.
type Corresp a b = Bila POS a b -> HLex IsBase -> LexSet

-- | We provide three component types, `Core`, `Filter` and `Choice`, which
-- can be combined together using the `buildCorresp` function to construct
-- a `Corresp` function.  The first one, `Core`, is used to identify a list
-- of potential sets of lexemes.  It is natural to define the core function
-- in such a way because the task of determining corresponding lexemes can
-- be usually divided into a set of smaller tasks with the same purpose.
-- For example, we may want to identify @LexSet@s corresponding to individual
-- word forms of the historical lexeme.
type Core a b = Bila POS a b -> HLex IsBase -> [LexSet]

-- | Function which can be used to filter out lexemes which do not
-- satisfy a particular predicate.  For example, we may want to filter
-- out lexemes with incompatible POS value.
type Filter = HLex IsBase -> (LexID, S.Set Word) -> Bool

-- | The final choice of lexemes.  Many different strategies can be used
-- here -- sum of the sets, intersection, or voting.
type Choice   = [LexSet] -> LexSet

-- | Identify @LexSet@s corresponding to individual word forms of the
-- historical lexeme using the `byForm` function.
byForms :: Core a b
byForms bila HLex{..} =
    [ withForm bila word
    | word <- M.keys hWords ]

-- | Identify lexemes which contain given word form.
withForm :: Bila POS a b -> Word -> LexSet
withForm Bila{..} word = M.fromList
    [ ( (base, pos) 
      , S.fromList
        [ word'
        | (_, (_, wordMap)) <- lookupL base baseDict
        , word' <- M.keys wordMap ] )
    | (pos, (_, baseMap)) <- lookupL word formDict
    , base <- M.keys baseMap ]
  where
    lookupL x = M.assocs . lookup x

-- | Filter out lexemes with POS value incompatible with the
-- set of POS values assigned to the historical lexeme.
posFilter :: Filter
posFilter HLex{..} ((_, pos), _) = pos `S.member` hPOSs

-- | Sum of sets of lexemes.
sumChoice :: Choice
sumChoice = M.unions

-- | Build the `Corresp` function form the individual components.
buildCorresp :: Core a b -> Filter -> Choice -> Corresp a b
buildCorresp core filt choice bila hLex
    = choice
    . map filterSet
    . core bila
    $ hLex
  where
    filterSet :: LexSet -> LexSet
    filterSet
        = M.fromList
        . filter (filt hLex)
        . M.assocs

-- | Code of origin.
data Code
    = Orig  -- ^ original (was already present in @HLex@)
    | Copy  -- ^ a copy (from corresponding lexeme) 
    deriving (Show, Eq, Ord)

instance Binary Code where
    put Orig = put '1'
    put Copy = put '2'
    get = get >>= \x -> return $ case x of
        '1' -> Orig
        '2' -> Copy
        c   -> error $ "get: invalid Code value '" ++ [c] ++ "'"

-- | Extend lexeme with forms from the set of lexemes.
extend :: HLex a -> LexSet -> HLex Code
extend HLex{..} lexSet = HLex hKey hUID hPOSs . M.fromList $
    [ (word, Orig)
    | (word, _) <- M.assocs hWords ]
        ++
    [ (word, Copy)
    | (_, wordSet) <- M.assocs lexSet
    , word <- S.elems wordSet ]

-- | Fuse the historical dictionary with bilateral contemporary
-- dictionary using the given `Corresp` function to determine
-- contemporary lexemes corresponding to individual lexemes
-- from historical dictionary.
fuse :: Corresp a b -> Hist -> Bila POS a b -> Dict UID () Code
fuse corr hist bila = mkDict
    [ (hKey, hUID, (), word, code)
    | hLex <- enumHist hist
    , let HLex{..} = extend hLex (corr bila hLex)
    , (word, code) <- M.assocs hWords ]
