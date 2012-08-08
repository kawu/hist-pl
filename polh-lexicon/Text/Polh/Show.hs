{-# LANGUAGE OverloadedStrings #-}

module Text.Polh.Show
( showPolh
, showLexEntry
) where

import Data.Monoid (Monoid, mempty, mappend, mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L

import Data.Polh.Types

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

-- | TODO: Print prolog and epilog.
showPolh :: Polh -> L.Text
showPolh =
    L.toLazyText . mconcat . map (<> "\n") . concatMap buildLexEntry

showLexEntry :: LexEntry -> L.Text
showLexEntry =
    L.toLazyText . mconcat . map (<> "\n") . buildLexEntry

-- | Each output line is represented as a builder. We use separate builders
-- for separate lines because we want to easilly indent the output text.
buildLexEntry :: LexEntry -> [L.Builder]
buildLexEntry lex =
    tagBeg : map ident body ++ [tagEnd]
  where
    tagBeg = "<LexicalEntry id=\"" <> L.fromText (lexId lex) <> "\">"
    tagEnd = "</LexicalEntry>"
    body
        =  buildLemma (lemma lex)
        ++ concatMap buildForm (forms lex)
        ++ concatMap buildRelForm (related lex)
        ++ concatMap buildSense (senses lex)

buildLemma :: Lemma -> [L.Builder]
buildLemma lemma =
    tagBeg : map ident body ++ [tagEnd]
  where
    tagBeg = "<Lemma>"
    tagEnd = "</Lemma>"
    body = concatMap (buildRepr "FormRepresentation") (repr lemma)

buildForm :: WordForm -> [L.Builder]
buildForm form =
    tagBeg : map ident body ++ [tagEnd]
  where
    tagBeg = "<WordForm>"
    tagEnd = "</WordForm>"
    body = concatMap (buildRepr "FormRepresentation") (repr form)

buildRelForm :: RelForm -> [L.Builder]
buildRelForm form =
    tagBeg : map ident body ++ [tagEnd]
  where
    tagBeg = "<RelatedForm targets=\"" <> L.fromText (relTo form) <> "\">"
    tagEnd = "</RelatedForm>"
    body = concatMap (buildRepr "FormRepresentation") (repr form)

buildSense :: Sense -> [L.Builder]
buildSense sense =
    tagBeg : map ident body ++ [tagEnd]
  where
    tagBeg = "<Sense>"
    tagEnd = "</Sense>"
    body
        =  concatMap buildDef (defs sense)
        ++ concatMap buildCxt (cxts sense)

buildDef :: Definition -> [L.Builder]
buildDef def =
    tagBeg : map ident body ++ [tagEnd]
  where
    tagBeg = "<Definition>"
    tagEnd = "</Definition>"
    body = concatMap (buildRepr "TextRepresentation") (repr def)

buildCxt :: Context -> [L.Builder]
buildCxt cxt =
    tagBeg : map ident body ++ [tagEnd]
  where
    tagBeg = "<Context>"
    tagEnd = "</Context>"
    body = concatMap (buildRepr "TextRepresentation") (repr cxt)

buildRepr :: L.Builder -> Repr -> [L.Builder]
buildRepr tag repr =
    tagBeg : map ident body ++ [tagEnd]
  where
    tagBeg = "<"  <> tag <> ">"
    tagEnd = "</" <> tag <> ">"
    body =
        [ buildFeat "writtenForm" (writtenForm repr)
        , buildFeat "language" (language repr)
        , buildFeat "sourceID" (sourceID repr) ]

buildFeat :: L.Builder -> T.Text -> L.Builder
buildFeat att val =
    "<feat att=\"" <> att <> "\" val=\"" <> L.fromText val <> "\"/>"
