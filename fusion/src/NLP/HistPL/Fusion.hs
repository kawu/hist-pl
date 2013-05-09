{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module NLP.HistPL.Fusion
(
-- * Basic types
  UID
, POS
, Word
, Base
, IsBase

-- -- * Dictionary
-- -- ** Entry
-- , Lex (..)
-- , LexKey (..)
-- , LexElem (..)
-- , LexSet
-- , mkLexSet
-- , unLexSet
-- -- ** Dictionary
-- , Dict
-- , BaseDict
-- , FormDict
-- , mkDict
-- , unDict
-- , revDict
-- , lookup
-- , entries
-- -- ** Bilateral
-- , Bila (..)
-- , mkBila
-- , withForm
-- -- ** Historical
-- , Hist
-- , mkHist
-- , HLex
-- -- ** Contemporary
-- , Poli
-- , PLex
-- , PLexSet
-- , mkPoli
-- 
-- -- * Correspondence
-- , Corresp
-- , buildCorresp
-- -- ** Components
-- , Core
-- , Filter
-- , Choice
-- , byForms
-- , posFilter
-- , sumChoice
-- 
-- -- * Fusion
-- , Fused
-- , FLex
-- , Code (..)
-- , extend
-- , fuse
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
import           NLP.HistPL.Dict

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


-- | Dictionary keys represent base forms and rules transform base forms to
-- their corresponding word forms.  Info @a@ is assigned to every lexeme
-- and info @b@ to every word form.
type BaseDict i a b = Dict i a b


-- | Dictionary keys represent word forms and rules transform word forms to
-- their corresponding base forms.  Info @a@ is assigned to every lexeme
-- and info @b@ to every word form.
type FormDict i a b = Dict i a b


------------------------------------------------------------------------


-- | Bilateral dictionary.
data Bila i a b = Bila
    { baseDict  :: BaseDict i a b
    , formDict  :: FormDict i a b }
    deriving (Show, Eq, Ord)


-- | Make bilateral dictionary from a list of (base form, ID, additional
-- lexeme info, word form, additional word form info) tuples.
mkBila :: (Ord i, Ord a, Ord b) => [(Base, i, a, Word, b)] -> Bila i a b
mkBila xs = Bila
    { baseDict  = baseDict'
    , formDict  = formDict' }
  where
    baseDict'   = fromList xs
    formDict'   = revDict baseDict'


-- | Identify entries which contain given word form.
withForm :: Ord i => Bila i a b -> Word -> Val i a Word b
withForm Bila{..} word = M.unions
    [ lookup base baseDict
    | let val = lookup word formDict
    , (_, (_, forms)) <- M.assocs val
    , base <- M.keys forms ]


------------------------------------------------------------------------


-- | PoliMorf dictionary in a bilateral form.
type Poli = Bila POS () ()


-- | PoliMorf dictionary entry.
type PLex = Entry POS () Word ()


-- | Set of PoliMorf dictionary entries.
type PVal = Val POS () Word ()


-- | Make bilateral dictionary from PoliMorf.
mkPoli :: [P.Entry] -> Poli
mkPoli = mkBila . map ((,,(),,()) <$> P.base <*> P.pos <*> P.form)


------------------------------------------------------------------------

-- The code below is redundant.  We don't want to create intermediate
-- DAWG historical dictionary representation.  We want to directly build
-- the resultant HistPL dictionary from a list of keyd lexemes.
--

-- -- | Historical dictionary.
-- type Hist = BaseDict UID (S.Set POS) IsBase
-- 
-- 
-- -- | Historical dictionary entry.
-- type HLex = Entry UID (S.Set POS) Word IsBase
-- 
-- 
-- -- | Construct historical dictionary.
-- mkHist :: [(H.Key, H.LexEntry)] -> Hist
-- mkHist xs = mkDict
--     [ ( keyForm, keyUid
--       , S.fromList (H.pos lexEntry)
--       , form, isBase )
--     | (key, lexEntry) <- xs
--     let H.Key{..} = key
--     , (form, isBase) <-
--         map (,True) (lemmas lexEntry) ++
--         map (,False) (forms lexEntry)
--     , oneWord form ]
--   where
--     lemmas = H.text . H.lemma
--     forms  = concatMap H.text . H.forms
--     oneWord = (==1) . length . T.words


------------------------------------------------------------------------

-- | A function which determines entries from a bilateral
-- dictionary corresponing to a given historical lexeme.
type Corresp = Poli -> HLex -> PVal

-- -- | We provide three component types, `Core`, `Filter` and `Choice`, which
-- -- can be combined together using the `buildCorresp` function to construct
-- -- a `Corresp` function.  The first one, `Core`, is used to identify a list
-- -- of potential sets of lexemes.  It is natural to define the core function
-- -- in such a way because the task of determining corresponding lexemes can
-- -- be usually divided into a set of smaller tasks of the same purpose.
-- -- For example, we may want to identify `LexSet`s corresponding to individual
-- -- word forms of the historical lexeme.
-- type Core = Poli -> HLex -> [PLexSet]
-- 
-- -- | Function which can be used to filter out lexemes which do not
-- -- satisfy a particular predicate.  For example, we may want to filter
-- -- out lexemes with incompatible POS value.
-- type Filter = HLex -> PLex -> Bool
-- 
-- -- | The final choice of lexemes.  Many different strategies can be used
-- -- here -- sum of the sets, intersection, or voting.
-- type Choice = [PLexSet] -> PLexSet
-- 
-- -- | Identify `LexSet`s corresponding to individual word forms of the
-- -- historical lexeme using the `withForm` function.
-- byForms :: Core
-- byForms bila Lex{..} =
--     [ withForm bila word
--     | word <- M.keys (forms lexElem) ]
-- 
-- -- | Filter out lexemes with POS value incompatible with the
-- -- set of POS values assigned to the historical lexeme.
-- posFilter :: Filter
-- posFilter h p = uid (lexKey p) `S.member` info (lexElem h)
-- 
-- -- | Sum of sets of lexemes.
-- sumChoice :: Choice
-- sumChoice = M.unions
-- 
-- -- | Build `Corresp` function form individual components.
-- buildCorresp :: Core -> Filter -> Choice -> Corresp
-- buildCorresp core filt choice bila hLex =
--     let filterSet = mkLexSet . filter (filt hLex) . unLexSet
--     in  choice . map filterSet . core bila $ hLex
-- 
-- ------------------------------------------------------------------------
-- 
-- -- | Fused dictionary.
-- type Fused = BaseDict UID () Code
-- 
-- -- | Fused dictionary entry.
-- type FLex = Lex UID () Code
-- 
-- -- | Code of word form origin.
-- data Code
--     = Orig  -- ^ original (was already present in `HLex`)
--     | Copy  -- ^ a copy (from corresponding lexeme)
--     deriving (Show, Eq, Ord)
-- 
-- instance Binary Code where
--     put Orig = put '1'
--     put Copy = put '2'
--     get = get >>= \x -> return $ case x of
--         '1' -> Orig
--         '2' -> Copy
--         c   -> error $ "get: invalid Code value '" ++ [c] ++ "'"
-- 
-- -- | Extend lexeme with forms from the set of lexemes.
-- extend :: HLex -> PLexSet -> FLex
-- extend hLex lexSet = subForms . M.fromList $
--     concatMap (fromElem Copy) (M.elems lexSet) ++
--     fromElem Orig (lexElem hLex)
--   where
--     subForms x = hLex { lexElem = LexElem () x }
--     fromElem code = map (,code) . (M.keys . forms)
-- 
-- -- | Fuse the historical dictionary with bilateral contemporary
-- -- dictionary using the given `Corresp` function to determine
-- -- contemporary lexemes corresponding to individual lexemes
-- -- from the historical dictionary.
-- fuse :: Corresp -> Hist -> Poli -> Fused
-- fuse corr hist bila = mkDict
--     [ (key, uid, (), word, code)
--     | hLex <- entries hist
--     , let Lex{..} = extend hLex (corr bila hLex)
--     , let LexKey{..} = lexKey
--     , (word, code) <- M.assocs (forms lexElem) ]
