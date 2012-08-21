{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Polh.Types
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
, Polh
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import Data.Text.Binary
import Data.Binary (Binary, put, get)

-- | Form or text representation.
data Repr = Repr
    { writtenForm :: T.Text
    , language    :: T.Text
    , sourceID    :: T.Text }
    deriving (Show, Read, Eq, Ord)

instance Binary Repr where
    put Repr{..} = do
        put writtenForm
        put language
        put sourceID
    get = Repr <$> get <*> get <*> get

class HasRepr t where
    repr :: t -> [Repr]

instance HasRepr [Repr] where
    repr = id

{-# INLINE text #-}
text :: HasRepr t => t -> [T.Text]
text = map writtenForm . repr

newtype WordForm = WordForm [Repr]
    deriving (Show, Read, Eq, Ord, Binary, HasRepr)

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

newtype Lemma = Lemma [Repr]
    deriving (Show, Read, Eq, Ord, Binary, HasRepr)

newtype Definition = Definition [Repr]
    deriving (Show, Read, Eq, Ord, Binary, HasRepr)

newtype Context = Context [Repr]
    deriving (Show, Read, Eq, Ord, Binary, HasRepr)

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

data Sense = Sense
    { senseId   :: Maybe T.Text
    , defs      :: [Definition]
    , cxts      :: [Context] }
    deriving (Show, Read, Eq, Ord)

instance Binary Sense where
    put Sense{..} = do
        put senseId
        put defs
        put cxts
    get = Sense <$> get <*> get <*> get

data LexEntry = LexEntry
    { lexId         :: T.Text
    , lemma         :: Lemma
    , forms         :: [WordForm]
    , components    :: [T.Text] -- ^ List of components
    , syntactic     :: [SynBehaviour]
    , senses        :: [Sense]
    , related       :: [RelForm] }
    deriving (Show, Read, Eq, Ord)

instance Binary LexEntry where
    put LexEntry{..} = do
        put lexId
        put lemma
        put forms
        put components
        put syntactic
        put senses
        put related
    get = LexEntry <$> get <*> get <*> get <*> get <*> get <*> get <*> get

type Polh = [LexEntry]
