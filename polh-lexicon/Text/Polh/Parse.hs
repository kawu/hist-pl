{-# LANGUAGE OverloadedStrings #-}

module Text.Polh.Parse
( parsePolh
, parseLexEntry
) where

import Control.Monad (join)
import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Text.XML.PolySoup as Soup
import Text.XML.PolySoup hiding (XmlParser, Parser, join)

import Data.Polh.Types

import Debug.Trace (trace)

type Parser a = Soup.XmlParser L.Text a

type Attr = T.Text
type AttrVal = T.Text

data LexContent
    = LexFeature Attr AttrVal
    | LexLemma Lemma
    | LexForm WordForm
    | LexCompo [T.Text]
    | LexRel RelForm
    -- | Syntactic Behaviour -- list of Repr structures and a list of
    -- sense identifiers, which should be processed to get direct access
    -- to a list of senses.
    | LexSyn ([Repr], [T.Text])
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

lexCompo :: LexContent -> Maybe [T.Text]
lexCompo (LexCompo compo) = Just compo
lexCompo _                = Nothing

lexRel :: LexContent -> Maybe RelForm
lexRel (LexRel rel) = Just rel
lexRel _            = Nothing

lexSyn :: LexContent -> Maybe ([Repr], [T.Text])
lexSyn (LexSyn syn) = Just syn
lexSyn _            = Nothing

lexSense :: LexContent -> Maybe Sense
lexSense (LexSense sense) = Just sense
lexSense _                = Nothing

lmfP :: Parser [LexEntry]
lmfP = true //> lexEntryP

lexEntryP :: Parser LexEntry
lexEntryP = (tag "LexicalEntry" *> getAttr "id") >^> \lexId -> do
    xs <- many $ oneOf
        [ uncurry LexFeature    <$> anyFeatP
        , LexLemma  <$> lemmaP
        , LexForm   <$> formP
        , LexCompo  <$> compoP
        , LexRel    <$> relP
        , LexSyn    <$> synP
        , LexSense  <$> senseP
        , LexOther  <$> otherP ]
    let fs = mapMaybe lexFeat xs
    let lemma = first "lexEntryP" (mapMaybe lexLemma xs)
    let forms = mapMaybe lexForm xs
    let compo = join (mapMaybe lexCompo xs)
    let senses = mapMaybe lexSense xs
    let syntactic =
            [ SynBehaviour reprs
                [ sense | sense <- senses
                , Just id <- [senseId sense]
                , id `elem` senseIds ]
            | (reprs, senseIds) <- mapMaybe lexSyn xs ]
    let related = mapMaybe lexRel xs
    return $ LexEntry
        { lexId = L.toStrict lexId
        , lemma = lemma
        , forms = forms
        , components = compo
        , syntactic = syntactic
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

compoP :: Parser [T.Text]
compoP = map L.toStrict <$> (tag "ListOfComponents" /> cut (getAttr "entry"))

relP :: Parser RelForm
relP = (tag "RelatedForm" *> getAttr "targets") >^> \relTo' -> do
    let relTo = L.toStrict relTo'
    rs <- many reprP
    return $ RelForm
        { relRepr = rs
        , relTo   = relTo }

otherP :: Parser ()
otherP = tagOpenName >^> \name ->
    warning ("tag " ++ L.unpack name ++ " ignored") ignore

warning :: String -> Parser a -> Parser a
warning msg x = trace ("WARNING: " ++ msg) x

warning' :: String -> a -> Parser a
warning' msg x = warning msg (return x)

grave :: String -> Parser a -> Parser a
grave msg x = trace ("ERROR: " ++ msg) x

grave' :: String -> a -> Parser a
grave' msg x = grave msg (return x)

synP :: Parser ([Repr], [T.Text])
synP = tag "SyntacticBehaviour" *> getAttr "senses" >^> \senses -> do
    repr <- reprBodyP
    return ([repr], T.words (L.toStrict senses))

data SenseContent
    = SenseDef Definition
    | SenseCxt Context
    | SenseOther ()

senseDef :: SenseContent -> Maybe Definition
senseDef (SenseDef def) = Just def
senseDef _              = Nothing

senseCxt :: SenseContent -> Maybe Context
senseCxt (SenseCxt cxt) = Just cxt
senseCxt _              = Nothing

senseP :: Parser Sense
senseP = tag "Sense" *> maybeAttr "id" >^> \senseId -> do
    xs <- many $ oneOf
        [ SenseDef      <$> defP
        , SenseCxt      <$> cxtP
        , SenseOther    <$> otherP ]
    let defs = mapMaybe senseDef xs
    let cxts = mapMaybe senseCxt xs
    return $ Sense
        { senseId = L.toStrict <$> senseId
        , defs = defs
        , cxts = cxts }

defP :: Parser Definition
defP = Definition <$> (tag "Definition" /> reprP)

cxtP :: Parser Context
cxtP = Context <$> (tag "Context" /> reprP)

reprP :: Parser Repr
reprP = tag "FormRepresentation" <|> tag "TextRepresentation" ^> reprBodyP

reprBodyP :: Parser Repr
reprBodyP = Repr
    <$> featP "writtenForm"
    <*> (featP "language" <|> grave' "language not specified" "polh")
    <*> (featP "sourceID" <|> grave' "sourceID not specified" "srpsdp")

anyFeatP :: Parser (T.Text, T.Text)
anyFeatP = cut $ tag "feat" *> ( (,)
    <$> (L.toStrict <$> getAttr "att")
    <*> (L.toStrict <$> getAttr "val") )

featP :: L.Text -> Parser T.Text
featP att = L.toStrict <$>
    cut (tag "feat" *> hasAttr "att" att *> getAttr "val")

parsePolh :: L.Text -> Polh
parsePolh = parseXML lmfP

parseLexEntry :: L.Text -> LexEntry
parseLexEntry = parseXML lexEntryP
