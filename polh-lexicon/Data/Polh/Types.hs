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
, Sense (..)
, LexEntry (..)
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

data Sense = Sense
    { defs  :: [Definition]
    , cxts  :: [Context] }
    deriving (Show, Read, Eq, Ord)

instance Binary Sense where
    put Sense{..} = do
        put defs
        put cxts
    get = Sense <$> get <*> get

data LexEntry = LexEntry
    { lemma     :: Lemma
    , forms     :: [WordForm]
    , senses    :: [Sense]
    , related   :: [RelForm] }
    deriving (Show, Read, Eq, Ord)

instance Binary LexEntry where
    put LexEntry{..} = do
        put lemma
        put forms
        put senses
        put related
    get = LexEntry <$> get <*> get <*> get <*> get
