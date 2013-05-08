{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module NLP.HistPL.Fusion
(
-- * Rule
  Rule (..)
, apply
, between

-- * Basic types
, UID
, POS
, Word
, Base
, IsBase

-- * Dictionary
-- ** Entry
, Lex (..)
, LexKey (..)
, LexElem (..)
, LexSet
, mkLexSet
, unLexSet
-- ** Dictionary
, Dict
, BaseDict
, FormDict
, mkDict
, unDict
, revDict
, lookup
, entries
-- ** Bilateral
, Bila (..)
, mkBila
, withForm
-- ** Historical
, Hist
, mkHist
, HLex
-- ** Contemporary
, Poli
, PLex
, PLexSet
, mkPoli

-- * Correspondence
, Corresp
, buildCorresp
-- ** Components
, Core
, Filter
, Choice
, byForms
, posFilter
, sumChoice

-- * Fusion
, Fused
, Code (..)
, extend
, fuse
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.Binary (Binary, get, put)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.PoliMorf as P
import qualified Data.DAWG.Static as D

import qualified NLP.HistPL as H

-- | A rule for translating a form into another form.
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

-- | Is the word form a base form?
type IsBase = Bool

------------------------------------------------------------------------

-- | A lexical entry.
data Lex i a b = Lex
    { lexKey    :: LexKey i
    , lexElem   :: LexElem a b }
    deriving (Show, Eq, Ord)

-- | Transform entry into a list.
listLex :: Lex i a b -> [(T.Text, i, a, T.Text, b)]
listLex Lex{..} =
    [ (key, uid, info, word, y)
    | (word, y) <- M.assocs forms ]
  where
    LexKey{..}  = lexKey
    LexElem{..} = lexElem

-- | Lexical entry dictionary key.
data LexKey i = LexKey
    { key   :: T.Text
    , uid   :: i }
    deriving (Show, Eq, Ord)

-- | Lexical entry info.
data LexElem a b = LexElem
    { info  :: a
    , forms :: M.Map Word b }
    deriving (Show, Eq, Ord)

-- | A set of lexical entries in a map form.
type LexSet i a b = M.Map (LexKey i) (LexElem a b) 

-- | Make lexical set from a list of entries.
mkLexSet :: Ord i => [Lex i a b] -> LexSet i a b
mkLexSet = M.fromList . map ((,) <$> lexKey <*> lexElem)

-- | List lexical entries.
unLexSet :: LexSet i a b -> [Lex i a b]
unLexSet = map (uncurry Lex) . M.toList

------------------------------------------------------------------------

type RuleEntry i a b = M.Map i (a, M.Map Rule b)

-- | One-way dictionary parametrized over ID @i@, with info @a@ for every
-- (key, i) pair and info @b@ for every (key, i, apply rule key) triple.
type Dict i a b = D.DAWG Char () (M.Map i (a, M.Map Rule b))

-- | Dictionary keys represent base forms and rules transform base forms to
-- their corresponding word forms.  Info @a@ is assigned to every lexeme
-- and info @b@ to every word form.
type BaseDict i a b = Dict i a b

-- | Dictionary keys represent word forms and rules transform word forms to
-- their corresponding base forms.  Info @a@ is assigned to every lexeme
-- and info @b@ to every word form.
type FormDict i a b = Dict i a b

-- | Decode dictionary entry.
decode :: Ord i => T.Text -> RuleEntry i a b -> LexSet i a b
decode key ruleEntry = mkLexSet
    [ Lex
        (LexKey key i)
        (LexElem x $ M.fromList
            [ (apply rule key, y)
            | (rule, y) <- M.assocs ruleMap ])
    | (i, (x, ruleMap)) <- M.assocs ruleEntry ]

-- | Lookup the key in the dictionary.
lookup :: Ord i => T.Text -> Dict i a b -> LexSet i a b
lookup key dict = decode key $ case D.lookup (T.unpack key) dict of
    Just m  -> m
    Nothing -> M.empty

-- | List dictionary lexical entries.
entries :: Ord i => Dict i a b -> [Lex i a b]
entries =
    let f = unLexSet . uncurry decode . first T.pack 
    in  concatMap f . D.assocs

-- | Make dictionary from a list of (key, ID, key\/ID info, elem,
-- key\/ID\/elem info) tuples.
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
unDict = concatMap listLex . entries

-- | Reverse the dictionary.
revDict :: (Ord i, Ord a, Ord b) => Dict i a b -> Dict i a b
revDict = 
    let swap (base, i, x, form, y) = (form, i, x, base, y)
    in  mkDict . map swap . unDict

------------------------------------------------------------------------

-- | Bilateral dictionary.
data Bila i a b = Bila
    { baseDict  :: BaseDict i a b
    , formDict  :: FormDict i a b }
    deriving (Show, Eq, Ord)

instance (Ord i, Binary i, Binary a, Binary b) => Binary (Bila i a b) where
    put Bila{..} = put baseDict >> put formDict
    get = Bila <$> get <*> get

-- | Make bilateral dictionary from a list of (base form, ID, additional
-- lexeme info, word form, additional word form info) tuples.
mkBila :: (Ord i, Ord a, Ord b) => [(Base, i, a, Word, b)] -> Bila i a b
mkBila xs = Bila
    { baseDict  = baseDict'
    , formDict  = formDict' }
  where
    baseDict'   = mkDict xs
    formDict'   = revDict baseDict'

-- | Identify entries which contain given word form.
withForm :: Ord i => Bila i a b -> Word -> LexSet i a b
withForm Bila{..} word = M.unions
    [ lookup base baseDict
    | (_, lexElem) <- M.assocs (lookup word formDict)
    , base <- M.keys (forms lexElem) ]

------------------------------------------------------------------------

-- | PoliMorf dictionary in a bilateral form.
type Poli = Bila POS () ()

-- | PoliMorf dictionary entry.
type PLex = Lex POS () ()

-- | Set of PoliMorf dictionary entries.
type PLexSet = LexSet POS () ()

-- | Make bilateral dictionary from PoliMorf.
mkPoli :: [P.Entry] -> Poli
mkPoli = mkBila . map ((,,(),,()) <$> P.base <*> P.pos <*> P.form)

------------------------------------------------------------------------

-- | Historical dictionary.
type Hist = BaseDict UID (S.Set POS) IsBase

-- | Historical dictionary entry.
type HLex = Lex UID (S.Set POS) IsBase

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
    , let entry = H.lexEntry binEntry
    , (form, isBase) <-
        map (,True) (lemmas entry) ++
        map (,False) (forms entry)
    , oneWord form ]
  where
    lemmas = H.text . H.lemma
    forms  = concatMap H.text . H.forms
    oneWord = (==1) . length . T.words

------------------------------------------------------------------------

-- | A function which determines entries from a bilateral
-- dictionary corresponing to a given historical lexeme.
type Corresp = Poli -> HLex -> PLexSet

-- | We provide three component types, `Core`, `Filter` and `Choice`, which
-- can be combined together using the `buildCorresp` function to construct
-- a `Corresp` function.  The first one, `Core`, is used to identify a list
-- of potential sets of lexemes.  It is natural to define the core function
-- in such a way because the task of determining corresponding lexemes can
-- be usually divided into a set of smaller tasks of the same purpose.
-- For example, we may want to identify `LexSet`s corresponding to individual
-- word forms of the historical lexeme.
type Core = Poli -> HLex -> [PLexSet]

-- | Function which can be used to filter out lexemes which do not
-- satisfy a particular predicate.  For example, we may want to filter
-- out lexemes with incompatible POS value.
type Filter = HLex -> PLex -> Bool

-- | The final choice of lexemes.  Many different strategies can be used
-- here -- sum of the sets, intersection, or voting.
type Choice = [PLexSet] -> PLexSet

-- | Identify `LexSet`s corresponding to individual word forms of the
-- historical lexeme using the `withForm` function.
byForms :: Core
byForms bila Lex{..} =
    [ withForm bila word
    | word <- M.keys (forms lexElem) ]

-- | Filter out lexemes with POS value incompatible with the
-- set of POS values assigned to the historical lexeme.
posFilter :: Filter
posFilter h p = uid (lexKey p) `S.member` info (lexElem h)

-- | Sum of sets of lexemes.
sumChoice :: Choice
sumChoice = M.unions

-- | Build `Corresp` function form individual components.
buildCorresp :: Core -> Filter -> Choice -> Corresp
buildCorresp core filt choice bila hLex =
    let filterSet = mkLexSet . filter (filt hLex) . unLexSet
    in  choice . map filterSet . core bila $ hLex

------------------------------------------------------------------------

-- | Fused dictionary.
type Fused = BaseDict UID () Code

-- | Fused dictionary entry.
type FLex = Lex UID () Code

-- | Code of word form origin.
data Code
    = Orig  -- ^ original (was already present in `HLex`)
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
extend :: HLex -> PLexSet -> FLex
extend hLex lexSet = subForms . M.fromList $
    concatMap (fromElem Copy) (M.elems lexSet) ++
    fromElem Orig (lexElem hLex)
  where
    subForms x = hLex { lexElem = LexElem () x }
    fromElem code = map (,code) . (M.keys . forms)

-- | Fuse the historical dictionary with bilateral contemporary
-- dictionary using the given `Corresp` function to determine
-- contemporary lexemes corresponding to individual lexemes
-- from the historical dictionary.
fuse :: Corresp -> Hist -> Poli -> Fused
fuse corr hist bila = mkDict
    [ (key, uid, (), word, code)
    | hLex <- entries hist
    , let Lex{..} = extend hLex (corr bila hLex)
    , let LexKey{..} = lexKey
    , (word, code) <- M.assocs (forms lexElem) ]
