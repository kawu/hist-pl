{-# LANGUAGE OverloadedStrings #-}

-- | Printing utilities for the LMF dictionary format.

module NLP.HistPL.LMF.Show
( showLMF
, showLexEntry
) where

import           Data.Monoid (Monoid, mappend, mconcat)
import           Data.List (intersperse)
import           Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import           Text.XML.PolySoup (escapeXml)

import           NLP.HistPL.Types

-- | An infix synonym for 'mappend'.
{-# INLINE (<>) #-}
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- | Indentation parameter.
indentSize :: Int
indentSize = 2

identPref :: L.Builder
identPref = L.fromLazyText (L.replicate (fromIntegral indentSize) " ")

{-# INLINE ident #-}
ident :: L.Builder -> L.Builder
ident = (identPref <>)

prolog :: [L.Builder]
prolog =
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<LexicalResource dtdVersion=\"16\">"
    , "  <GlobalInformation>"
    , "    <feat att=\"languageCoding\" val=\"ISO 639-6\"/>"
    , "  </GlobalInformation>"
    , "  <Lexicon>" ]

epilog :: [L.Builder]
epilog =
    [ "  </Lexicon>"
    , "</LexicalResource>" ]

-- | Show the entire dictionary as a lazy text in the LMF format.
showLMF :: [LexEntry] -> L.Text
showLMF =
    L.toLazyText . mconcat . map (<> "\n") . embed . concatMap buildLexEntry
    where embed body = prolog ++ map (ident.ident) body ++ epilog

-- | Show lexical entry using the LMF format.
showLexEntry :: LexEntry -> L.Text
showLexEntry =
    L.toLazyText . mconcat . map (<> "\n") . buildLexEntry

buildElem :: L.Builder -> [L.Builder] -> L.Builder -> [L.Builder]
buildElem beg body end = beg : map ident body ++ [end] 

-- | Each output line is represented as a builder. We use separate builders
-- for separate lines because we want to easilly indent the output text.
buildLexEntry :: LexEntry -> [L.Builder]
buildLexEntry lx =
    buildElem beg body end
  where
    beg = "<LexicalEntry id=\"" <> L.fromText (lexID lx) <> "\">"
    end = "</LexicalEntry>"
    body
        =  map (buildFeat "lineRef") (maybeToList $ lineRef lx)
        ++ map (buildFeat "status") (maybeToList $ status lx)
        ++ map (buildFeat "partOfSpeech") (pos lx)
        ++ buildLemma (lemma lx)
        ++ concatMap buildForm (forms lx)
        ++ concatMap buildRelForm (related lx)
        ++ buildComps (components lx)
        ++ concatMap buildSyn (syntactic lx)
        ++ concatMap buildSense (senses lx)

buildLemma :: Lemma -> [L.Builder]
buildLemma base =
    buildElem beg body end
  where
    beg = "<Lemma>"
    end = "</Lemma>"
    body = concatMap (buildRepr "FormRepresentation") (repr base)

buildForm :: WordForm -> [L.Builder]
buildForm form =
    buildElem beg body end
  where
    beg = "<WordForm>"
    end = "</WordForm>"
    body = concatMap (buildRepr "FormRepresentation") (repr form)

buildRelForm :: RelForm -> [L.Builder]
buildRelForm form =
    buildElem beg body end
  where
    beg = "<RelatedForm targets=\"" <> L.fromText (relTo form) <> "\">"
    end = "</RelatedForm>"
    body = concatMap (buildRepr "FormRepresentation") (repr form)

buildComps :: [T.Text] -> [L.Builder]
buildComps [] = []
buildComps xs =
    buildElem beg body end
  where
    beg = "<ListOfComponents>"
    end = "</ListOfComponents>"
    body = map comp xs
    comp x = "<Component entry=\"" <> L.fromText x <> "\"/>"

buildSyn :: SynBehaviour -> [L.Builder]
buildSyn syn =
    buildElem beg body end
  where
    ids = mconcat . intersperse " " . map L.fromText $ synSenseIds syn
    beg = "<SyntacticBehaviour senses=\"" <> ids <> "\">"
    end = "</SyntacticBehaviour>"
    body = concatMap (buildRepr "TextRepresentation") (repr syn)

buildSense :: Sense -> [L.Builder]
buildSense sense =
    buildElem beg body end
  where
    beg = case senseId sense of
        Just x  -> "<Sense id=\"" <> L.fromText x <> "\">"
        Nothing -> "<Sense>"
    end = "</Sense>"
    body
        =  map (buildFeat "style") (style sense)
        ++ concatMap buildDef (defs sense)
        ++ concatMap buildCxt (cxts sense)

buildDef :: Definition -> [L.Builder]
buildDef def =
    buildElem beg body end
  where
    beg = "<Definition>"
    end = "</Definition>"
    body = concatMap (buildRepr "TextRepresentation") (repr def)

buildCxt :: Context -> [L.Builder]
buildCxt cxt =
    buildElem beg body end
  where
    beg = "<Context>"
    end = "</Context>"
    body = concatMap (buildRepr "TextRepresentation") (repr cxt)

buildRepr :: L.Builder -> Repr -> [L.Builder]
buildRepr tag rp =
    buildElem beg body end
  where
    beg = "<"  <> tag <> ">"
    end = "</" <> tag <> ">"
    body =
        [ buildFeat "writtenForm" . escapeXml $ writtenForm rp
        , buildFeat "language" (language rp) ] ++ source
    source = case sourceID rp of
        Just x  -> [buildFeat "sourceID" x]
        Nothing -> []

buildFeat :: L.Builder -> T.Text -> L.Builder
buildFeat att val =
    "<feat att=\"" <> att <> "\" val=\"" <> L.fromText val <> "\"/>"
