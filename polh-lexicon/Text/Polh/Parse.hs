{-# LANGUAGE OverloadedStrings #-}

module Text.Polh.Parse
( parsePolh
, parseLexEntry
) where

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Text.XML.PolySoup as Soup
import Text.XML.PolySoup hiding (XmlParser, Parser)

import Data.Polh.Types

import Debug.Trace (trace)

type Parser a = Soup.XmlParser L.Text a

type Attr = T.Text
type AttrVal = T.Text

data LexContent
    = LexFeature Attr AttrVal
    | LexLemma Lemma
    | LexForm WordForm
    | LexRel RelForm
    | LexSense Sense
    | LexOther ()

lexFeat :: LexContent -> Maybe (Attr, AttrVal)
lexFeat (LexFeature attr val) = Just (attr, val)
lexFeat _                     = Nothing

lexLemma :: LexContent -> Maybe Lemma
lexLemma (LexLemma lemma) = Just lemma
lexLemma _                = Nothing

lexForm :: LexContent -> Maybe WordForm
lexForm (LexForm form) = Just form
lexForm _              = Nothing

lexRel :: LexContent -> Maybe RelForm
lexRel (LexRel rel) = Just rel
lexRel _            = Nothing

lexSense :: LexContent -> Maybe Sense
lexSense (LexSense sense) = Just sense
lexSense _                = Nothing

lmfP :: Parser [LexEntry]
lmfP = true //> lexEntryP

lexEntryP :: Parser LexEntry
lexEntryP = (tag "LexicalEntry" *> getAttr "id") `join` \lexId -> do
    xs <- many $ oneOf
        [ uncurry LexFeature    <$> anyFeatP
        , LexLemma  <$> lemmaP
        , LexForm   <$> formP
        , LexRel    <$> relP
        , LexSense  <$> senseP
        , LexOther  <$> lexOtherP ]
    let fs = mapMaybe lexFeat xs
    let lemma = first "lexEntryP" (mapMaybe lexLemma xs)
    let forms = mapMaybe lexForm xs
    let senses = mapMaybe lexSense xs
    let related = mapMaybe lexRel xs
    return $ LexEntry
        { lemma = lemma
        , forms = forms
        , senses = senses
        , related = related }

first :: Show a => String -> [a] -> a
first src [x] = x
first src []  = error $ src ++ ": null xs"
first src xs  = error $ src ++ ": xs == " ++ show xs

lemmaP :: Parser Lemma
lemmaP = Lemma <$> (tag "Lemma" /> reprP)

formP :: Parser WordForm
formP = WordForm <$> (tag "WordForm" /> reprP)

relP :: Parser RelForm
relP = (tag "RelatedForm" *> getAttr "targets") `join` \relTo' -> do
    let relTo = L.toStrict relTo'
    rs <- many reprP
    return $ RelForm
        { relRepr = rs
        , relTo   = relTo }

lexOtherP :: Parser ()
lexOtherP = tagOpenName `join` \name ->
    trace ("WARNING: tag " ++ L.unpack name ++ " ignored") ignore

data SenseContent
    = SenseDef Definition
    | SenseCxt Context

senseDef :: SenseContent -> Maybe Definition
senseDef (SenseDef def) = Just def
senseDef _              = Nothing

senseCxt :: SenseContent -> Maybe Context
senseCxt (SenseCxt cxt) = Just cxt
senseCxt _              = Nothing

senseP :: Parser Sense
senseP = tag "Sense" `joinR` do
    xs <- many $ oneOf
        [ SenseDef  <$> defP
        , SenseCxt  <$> cxtP ]
    let defs = mapMaybe senseDef xs
    let cxts = mapMaybe senseCxt xs
    return $ Sense
        { defs = defs
        , cxts = cxts }

defP :: Parser Definition
defP = Definition <$> (tag "Definition" /> reprP)

cxtP :: Parser Context
cxtP = Context <$> (tag "Context" /> reprP)

reprP :: Parser Repr
reprP = (tag "FormRepresentation" <|> tag "TextRepresentation") `joinR`
  ( Repr
    <$> featP "writtenForm"
    <*> featP "language"
    <*> featP "sourceID" )

anyFeatP :: Parser (T.Text, T.Text)
anyFeatP = cut $ tag "feat" *> ( (,)
    <$> (L.toStrict <$> getAttr "att")
    <*> (L.toStrict <$> getAttr "val") )

featP :: L.Text -> Parser T.Text
featP att = L.toStrict <$>
    cut (tag "feat" *> hasAttr "att" att *> getAttr "val")

parsePolh :: L.Text -> [LexEntry]
parsePolh = parseXML lmfP

parseLexEntry :: L.Text -> LexEntry
parseLexEntry = parseXML lexEntryP
