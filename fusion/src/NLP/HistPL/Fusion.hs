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

-- * Dictionary
-- ** Bilateral
, Bila (..)
, mkBila
, withForm
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
) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.Text.Binary ()
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.PoliMorf as P

import           NLP.HistPL (UID)
import qualified NLP.HistPL as H
import qualified NLP.HistPL.Util as H
import           NLP.HistPL.Dict

------------------------------------------------------------------------


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
withForm :: Ord i => Bila i a b -> Word -> LexSet i a b
withForm Bila{..} word = M.unions
    [ lookup base baseDict
    | let lexSet = lookup word formDict
    , (_, val) <- M.assocs lexSet
    , base <- M.keys (forms val) ]


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


-- -- | Historical dictionary.
-- type Hist = BaseDict UID (S.Set POS) IsBase
-- 
-- 
-- -- | Historical dictionary entry.
-- type HLex = Lex UID (S.Set POS) IsBase
-- 
-- 
-- -- | Construct historical dictionary.
-- mkHist :: [(H.Key, H.LexEntry)] -> Hist
-- mkHist xs = fromList
--     [ ( orth, uid
--       , S.fromList (H.pos lexEntry)
--       , form, isBase )
--     | (key, lexEntry) <- xs
--     , let Key{..} = key
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
type Corresp = Poli -> H.LexEntry -> PLexSet


-- | We provide three component types, `Core`, `Filter` and `Choice`, which
-- can be combined together using the `buildCorresp` function to construct
-- a `Corresp` function.  The first one, `Core`, is used to identify a list
-- of potential sets of lexemes.  It is natural to define the core function
-- in such a way because the task of determining corresponding lexemes can
-- be usually divided into a set of smaller tasks of the same purpose.
-- For example, we may want to identify `LexSet`s corresponding to individual
-- word forms of the historical lexeme.
type Core = Poli -> H.LexEntry -> [PLexSet]


-- | Function which can be used to filter out lexemes which do not
-- satisfy a particular predicate.  For example, we may want to filter
-- out lexemes with incompatible POS value.
type Filter = H.LexEntry -> PLex -> Bool


-- | The final choice of lexemes.  Many different strategies can be used
-- here sum of the sets, intersection, or voting.
type Choice = [PLexSet] -> PLexSet


-- | Identify `LexSet`s corresponding to individual word forms of the
-- historical lexeme using the `withForm` function.
byForms :: Core
byForms bila lexEntry =
    [ withForm bila word
    | word <- H.allForms lexEntry ]


-- | Filter out lexemes with POS value incompatible with the
-- set of POS values assigned to the historical lexeme.
posFilter :: Filter
posFilter h p = uid (lexKey p) `elem` H.pos h


-- | Sum of sets of lexemes.
sumChoice :: Choice
sumChoice = M.unions


-- | Build `Corresp` function form individual components.
buildCorresp :: Core -> Filter -> Choice -> Corresp
buildCorresp core filt choice bila hLex =
    let filterSet = mkLexSet . filter (filt hLex) . unLexSet
    in  choice . map filterSet . core bila $ hLex
