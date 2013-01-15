{-# LANGUAGE RecordWildCards #-}

module NLP.Polh.Test
(
-- * Rules
  Rule (..)
, apply
, between
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (maybeToList)
import Data.Binary (Binary, get, put)
import Data.Text.Binary ()
import Data.PoliMorf (POS)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.DAWG.Static as D

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

-- | Basic types.
type UID = Int
type Word = T.Text
type Base = T.Text
type IsBase = Bool

-- | DAWG with `String` keys.
type DAWG a = D.DAWG Char () a

-- | One-way dictionary parametrized over ID @i@, with info @a@ for every
-- (key, i) pair and info @b@ for every (key, i, apply rule key) triple.
type Dict i a b = DAWG (M.Map i (a, M.Map Rule b))

lookup :: Ord i => T.Text -> Dict i a b -> M.Map i (a, M.Map Word b)
lookup x'Text dict = M.fromList
    [ (i, (a, M.fromList
        [ (apply rule x'Text, b)
        | (rule, b) <- M.assocs ruleMap ]))
    | m <- lookup' x'Text dict
    , (i, (a, ruleMap)) <- M.assocs m ]
  where
    lookup' x = maybeToList . D.lookup (T.unpack x)

mkDict :: (Ord i, Ord a, Ord b) => [(T.Text, i, a, T.Text, b)] -> Dict i a b
mkDict xs = D.fromListWith union $
    [ ( T.unpack x
      , M.singleton i
        (a, M.singleton (between x y) b) )
    | (x, i, a, y, b) <- xs ]
  where
    union = M.unionWith $ both const M.union
    both f g (x, y) (x', y') = (f x x', g y y')

-- Dictionary keys include base forms and rules transform base forms to
-- their corresponding word forms.  Info @a@ is assigned to every lexeme
-- and info @b@ to every word form.
type BaseDict i a b = Dict i a b

-- Dictionary keys include word forms and rules transform word forms to
-- their corresponding base forms.  Info @a@ is assigned to every word
-- form.
type FormDict i a = Dict i () a

-- | Bilateral dictionary.
data Bila i a b = Bila
    { baseDict  :: BaseDict i a b
    , formDict  :: FormDict i b }
    deriving (Show, Eq, Ord)

-- | Historical dictionary.
type Hist = Dict UID (S.Set POS) IsBase

-- | Entry from historical dictionary.
data HLex a = HLex
    { hKey      :: T.Text
    , hUID      :: UID
    , hPOSs     :: S.Set POS
    , hWords    :: M.Map Word a }
    deriving (Show, Eq, Ord)

-- | List all lexical entries from historical dictionary.
histLexs :: Hist -> [HLex IsBase]
histLexs hist =
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
    | hLex <- histLexs hist
    , let HLex{..} = extend hLex (corr bila hLex)
    , (word, code) <- M.assocs hWords ]
