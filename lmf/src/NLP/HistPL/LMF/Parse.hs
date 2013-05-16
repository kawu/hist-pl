{-# LANGUAGE OverloadedStrings #-}

-- | The module provides parsing utilities for the LMF dictionary.

module NLP.HistPL.LMF.Parse
( readLMF
, parseLMF
, parseLexEntry
) where

import           Control.Monad (join)
import           Data.Maybe (mapMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Text.XML.PolySoup as Soup
import           Text.XML.PolySoup hiding (XmlParser, Parser, join)

import           NLP.HistPL.Types

import           Debug.Trace (trace)

type Parser a = Soup.XmlParser L.Text a

lmfP :: Parser [LexEntry]
lmfP = true //> lexEntryP

lexEntryP :: Parser LexEntry
lexEntryP = tag "LexicalEntry" *> getAttr "id" >^>
  \lexID' -> collTags >>=
  \tags   -> return $
    let with p = tagsParseXml (findAll p) tags
    in  LexEntry
        { lexID         = L.toStrict lexID'
        , lineRef       = listToMaybe $ with lineRefP
        , status        = listToMaybe $ with statusP
        , pos           = with posP
        , lemma         = first "lemmaP" (with lemmaP)
        , forms         = with formP
        , components    = join (with compoP)
        , syntactic     = with synP
        , senses        = with senseP
        , related       = with relP }
    
first :: Show a => String -> [a] -> a
first _   [x] = x
first src []  = error $ src ++ ": null xs"
first src xs  = error $ src ++ ": xs == " ++ show xs

posP :: Parser T.Text
posP = featP "partOfSpeech"

lineRefP :: Parser T.Text
lineRefP = featP "lineRef"

statusP :: Parser T.Text
statusP = featP "status"

lemmaP :: Parser Lemma
lemmaP = Lemma <$> (tag "Lemma" /> reprP)

formP :: Parser WordForm
formP = WordForm <$> (tag "WordForm" /> reprP)

compoP :: Parser [T.Text]
compoP = map L.toStrict <$> (tag "ListOfComponents" /> cut (getAttr "entry"))

relP :: Parser RelForm
relP = tag "RelatedForm" *> getAttr "targets" >^> \relTo' -> do
    rs <- many reprP
    return $ RelForm
        { relRepr = rs
        , relTo   = L.toStrict relTo' }

otherP :: Parser ()
otherP = tagOpenName >^> \name ->
    warning ("tag " ++ L.unpack name ++ " ignored") ignore

warning :: String -> Parser a -> Parser a
warning msg x = trace ("WARNING: " ++ msg) x

grave :: String -> Parser a -> Parser a
grave msg x = trace ("ERROR: " ++ msg) x

grave' :: String -> a -> Parser a
grave' msg x = grave msg (return x)

synP :: Parser SynBehaviour
synP = tag "SyntacticBehaviour" *> getAttr "senses" >^> \senses' -> do
    repr' <- reprBodyP
    let senseIds = T.words (L.toStrict senses')
    return (SynBehaviour [repr'] senseIds)

data SenseContent
    = SenseDef Definition
    | SenseStyle T.Text
    | SenseCxt Context
    | SenseOther ()

senseStyle :: SenseContent -> Maybe T.Text
senseStyle (SenseStyle x) = Just x
senseStyle _              = Nothing

senseDef :: SenseContent -> Maybe Definition
senseDef (SenseDef def) = Just def
senseDef _              = Nothing

senseCxt :: SenseContent -> Maybe Context
senseCxt (SenseCxt cxt) = Just cxt
senseCxt _              = Nothing

senseP :: Parser Sense
senseP = tag "Sense" *> maybeAttr "id" >^> \senseId' -> do
    xs <- many $ oneOf
        [ SenseDef      <$> defP
        , SenseStyle    <$> styleP
        , SenseCxt      <$> cxtP
        , SenseOther    <$> otherP ]
    let styl' = mapMaybe senseStyle xs
    let defs' = mapMaybe senseDef xs
    let cxts' = mapMaybe senseCxt xs
    return $ Sense
        { senseId = L.toStrict <$> senseId'
        , style = styl'
        , defs  = defs'
        , cxts  = cxts' }

defP :: Parser Definition
defP = Definition <$> (tag "Definition" /> reprP)

cxtP :: Parser Context
cxtP = Context <$> (tag "Context" /> reprP)

styleP :: Parser T.Text
styleP = featP "style"

reprP :: Parser Repr
reprP = tag "FormRepresentation" <|> tag "TextRepresentation" ^> reprBodyP

reprBodyP :: Parser Repr
reprBodyP = Repr
    <$> featP "writtenForm"
    <*> (featP "language" <|> grave' "language not specified" "polh")
    <*> (optional $ featP "sourceID")

featP :: L.Text -> Parser T.Text
featP att = L.toStrict <$>
    cut (tag "feat" *> hasAttr "att" att *> getAttr "val")

-- | Read the dictionary from the LMF file.
readLMF :: FilePath -> IO [LexEntry]
readLMF = fmap parseLMF . L.readFile

-- | Parse the entire dictionary in the LMF format.
parseLMF :: L.Text -> [LexEntry]
parseLMF = parseXml lmfP

-- | Parse the lexical entry LMF representation
parseLexEntry :: L.Text -> LexEntry
parseLexEntry = parseXml lexEntryP
