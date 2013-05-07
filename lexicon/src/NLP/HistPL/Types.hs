{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | The module provides types for dictionary representation.

module NLP.HistPL.Types
( Repr (..)
, HasRepr (..)
, text
, WordForm (..)
, Lemma (..)
, RelForm (..)
, Definition (..)
, Context (..)
, SynBehaviour (..)
, Sense (..)
, LexEntry (..)
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import Data.Text.Binary ()
import Data.Binary (Binary, put, get)

-- | Form or text representation.
data Repr = Repr
    { writtenForm :: T.Text
    , language    :: T.Text
    , sourceID    :: Maybe T.Text }
    deriving (Show, Read, Eq, Ord)

instance Binary Repr where
    put Repr{..} = do
        put writtenForm
        put language
        put sourceID
    get = Repr <$> get <*> get <*> get

-- | A class of objects with a written representation.
class HasRepr t where
    repr :: t -> [Repr]

instance HasRepr [Repr] where
    repr = id

-- | Get textual representations of an object.
text :: HasRepr t => t -> [T.Text]
text = map writtenForm . repr
{-# INLINE text #-}

-- | A word form.
newtype WordForm = WordForm [Repr]
    deriving (Show, Read, Eq, Ord, Binary, HasRepr)

-- | A related form.
data RelForm = RelForm
    { relRepr   :: [Repr]
    , relTo     :: T.Text }
    deriving (Show, Read, Eq, Ord)

instance Binary RelForm where
    put RelForm{..} = do
        put relRepr
        put relTo
    get = RelForm <$> get <*> get

instance HasRepr RelForm where
    repr = relRepr

-- | A lemma (base) form.
newtype Lemma = Lemma [Repr]
    deriving (Show, Read, Eq, Ord, Binary, HasRepr)

-- | A definition of the lexeme sense.
newtype Definition = Definition [Repr]
    deriving (Show, Read, Eq, Ord, Binary, HasRepr)

-- | A context in which a given sense is illustrated.
newtype Context = Context [Repr]
    deriving (Show, Read, Eq, Ord, Binary, HasRepr)

-- | A description of a syntactic behaviour.
data SynBehaviour = SynBehaviour
    { synRepr     :: [Repr]
    , synSenseIds :: [T.Text] }
    deriving (Show, Read, Eq, Ord)

instance HasRepr SynBehaviour where
    repr = synRepr

instance Binary SynBehaviour where
    put SynBehaviour{..} = do
        put synRepr
        put synSenseIds
    get = SynBehaviour <$> get <*> get

-- | A potential sense of a given lexeme.
data Sense = Sense
    { senseId   :: Maybe T.Text
    , style     :: [T.Text]
    , defs      :: [Definition]
    , cxts      :: [Context] }
    deriving (Show, Read, Eq, Ord)

instance Binary Sense where
    put Sense{..} = do
        put senseId
        put style
        put defs
        put cxts
    get = Sense <$> get <*> get <*> get <*> get

-- | A description of a lexeme.
data LexEntry = LexEntry {
    -- | An ID of the lexical entry.
      lexId         :: T.Text
    -- | A line reference number.  Provisional field.
    , lineRef       :: Maybe T.Text
    -- | A status of the lexeme.  Provisional field.
    , status        :: Maybe T.Text
    -- | Potential parts of speech.
    , pos           :: [T.Text]
    -- | A base form.
    , lemma         :: Lemma
    -- | Word forms of the lexeme.
    , forms         :: [WordForm]
    -- | A list of components (only when the entry represent
    -- a compound lexeme).
    , components    :: [T.Text]
    -- | A list of potential syntactic behaviours of the lexeme.
    , syntactic     :: [SynBehaviour]
    -- | A list of potential semantic descriptions.
    , senses        :: [Sense]
    -- | Forma related to the lexeme.
    , related       :: [RelForm] }
    deriving (Show, Read, Eq, Ord)

instance Binary LexEntry where
    put LexEntry{..} = do
        put lexId
        put lineRef
        put status
        put pos
        put lemma
        put forms
        put components
        put syntactic
        put senses
        put related
    get = LexEntry <$> get <*> get <*> get <*> get <*> get
                   <*> get <*> get <*> get <*> get <*> get
