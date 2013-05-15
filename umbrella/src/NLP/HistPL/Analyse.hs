{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | The module provides functions for dictionary-driven analysis
-- of the input text.


module NLP.HistPL.Analyse
( Token (..)
, Other (..)
, tokenize
, anaText
, anaWord
, mapL
, showAna
) where


import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend, mconcat)
import Data.Ord (comparing)
import Data.List (sortBy, intersperse)
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L

import qualified NLP.Morfeusz as R
import qualified NLP.HistPL.Lexicon as H



-- | A token is an element of the analysis result.
data Token = Token {
    -- | Orthographic form.
      orth  :: T.Text
    -- | Historical interpretations.
    , hist  :: [(H.LexEntry, H.Code)]
    -- | Contemporary interpretations.
    , cont  :: [[R.Interp]] }
    deriving (Show)


-- | A punctuation or a space.
data Other
    -- | Punctuation
    = Pun T.Text
    -- | Space
    | Space T.Text
    deriving (Show)


-- | Perform simple tokenization -- spaces and punctuation
-- are treated as token ending markers.
tokenize :: T.Text -> [Either T.Text Other]
tokenize =
    map mkElem . T.groupBy cmp
  where
    cmp x y
        | C.isPunctuation x = False
        | C.isPunctuation y = False
        | otherwise         = C.isSpace x == C.isSpace y
    mkElem x
        | T.any C.isSpace x         = Right (Space x)
        | T.any C.isPunctuation x   = Right (Pun x)
        | otherwise                 = Left x


-- | Analyse the text.
anaText :: H.HistPL -> T.Text -> IO [Either Token Other]
anaText hpl = mapL (anaWord hpl) . tokenize


-- | Map the monadic function over left elements.
mapL :: (Functor m, Monad m) => (a -> m a') -> [Either a b] -> m [Either a' b]
mapL f =
    let g (Left x)  = Left <$> f x
        g (Right y) = return (Right y)
    in  mapM g


-- | Analyse the word.
anaWord :: H.HistPL -> T.Text -> IO Token
anaWord hpl x = do
    _hist <- H.lookupMany hpl [x, T.toLower x]
    _cont <- return (anaCont x)
    return $ Token x _hist _cont


-- -- | Analyse the word with respect to the historical dictionary.
-- anaHist :: Hist -> T.Text -> IO [(H.Code, H.LexEntry)]
-- anaHist hd word = sequence
--     [ (,) <$> follow (H.Key base uid) <*> pure code
--     | (H.LexKey{..}, H.LexElem{..}) <- M.assocs keys
--     , (base, code) <- M.assocs forms ]
--   where
--     -- Identify lexical entry with the given key.
--     follow = fmap (H.lexEntry . fromJust) . H.withKey (histPL hd)
--     -- Analyse both the original form and the lowercased form.
--     keys = M.unionWith (right (M.unionWith min))
--         (ana word)
--         (ana (T.toLower word))
--     right f (H.LexElem x y) (H.LexElem _ y') = H.LexElem x (f y y')
--     ana = flip H.lookup (fused hd)


-- | Analyse word using the Morfeusz analyser for contemporary Polish.
anaCont :: T.Text -> [[R.Interp]]
anaCont = map R.interps . head . R.paths . R.analyse False


-- | Show analysed text.
showAna :: [Either Token Other] -> L.Text
showAna = L.toLazyText . mconcat . newlineSep . buildAna


buildAna :: [Either Token Other] -> [L.Builder]
buildAna xs = "sent:" :
    map indent (concatMap (either buildTok buildOther) xs)


-- | List of Text builders for the token.  Individual lines are represented
-- by different builders.
buildTok :: Token -> [L.Builder]
buildTok tok
    =  buildHead tok
    :  map (indent . buildHist) histInterps
    -- ++ concatMap buildCont (cont tok)
  where
    histInterps = sortBy (comparing snd) (hist tok)


buildHead :: Token -> L.Builder
buildHead tok = "word: " <> L.fromText (orth tok)


-- | Build a list of historical interpretations.
buildHist :: (H.LexEntry, H.Code) -> L.Builder
buildHist (entry, code)
    =  "hist: " <> buildID (H.lexId entry, code)
    <> " "      <> buildPos
    <> ": "      <> commaRepr (H.lemma entry)
--     <> " " <> "tags: "
  where
    buildID (id', cd') = "[" <> L.fromText id' <> ", " <> buildCode cd' <> "]"
    buildPos = case H.pos entry of
        [] -> "-"
        xs -> mconcat . commaSep . map L.fromText $ xs
    buildCode code' = case code' of
        H.Orig -> "orig"
        H.Both -> "both"
        H.Copy -> "copy"


buildOther :: Other -> [L.Builder]
buildOther (Space _) = ["<space>"]
buildOther (Pun t)   = ["pun: " <> L.fromText t]


commaRepr :: H.HasRepr t => t -> L.Builder
commaRepr = mconcat . commaSep . map L.fromText . H.text


(<>) :: Monoid m => m -> m -> m
(<>) = mappend


indent :: L.Builder -> L.Builder
indent = ("  " <>)


commaSep :: [L.Builder] -> [L.Builder]
commaSep = intersperse ", "


newlineSep :: [L.Builder] -> [L.Builder]
newlineSep = intersperse "\n"
