{-# LANGUAGE OverloadedStrings #-}

module NLP.Polh.Analyse
( Hist
, Token (..)
, Other (..)
, tokenize
, anaText
, anaWord
, mapL
, showText
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

import qualified NLP.Polh.Types as H
import qualified NLP.Polh.Binary as H
import qualified NLP.Polh.Fusion as F

import qualified NLP.Morfeusz as R

type Hist = F.FormDict F.UID F.Code

data Token = Token {
    -- | Orthographic form.
      orth  :: T.Text
    -- | Historical interpretations.
    , hist  :: [(H.LexEntry, F.Code)]
    -- | Contemporary interpretations.
    , cont  :: [[R.Interp]] }
    deriving (Show)

data Other
    -- | Punctuation.
    = Pun T.Text
    -- | Space
    | Space T.Text
    deriving (Show)

-- | Perform the simple tokenization.
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
anaText :: Hist -> T.Text -> H.PolhM [Either Token Other]
anaText hd = mapL (anaWord hd) . tokenize

-- | Map the monadic function over left elements.
mapL :: (Functor m, Monad m) => (a -> m a') -> [Either a b] -> m [Either a' b]
mapL f =
    let g (Left x)  = Left <$> f x
        g (Right y) = return (Right y)
    in  mapM g

-- | Analyse the word.
anaWord :: Hist -> T.Text -> H.PolhM Token
anaWord hd x = do
    _hist <- anaHist hd x
    _cont <- return (anaCont x)
    return $ Token x _hist _cont

-- | Analyse the word with respect to the historical dictionary. 
anaHist :: Hist -> T.Text -> H.PolhM [(H.LexEntry, F.Code)]
anaHist hd word = sequence
    [ (,) <$> follow key <*> pure code
    | (uid, (_, baseMap)) <- M.assocs keys
    , (base, code) <- M.assocs baseMap
    , let key = H.Key base uid ]
  where
    -- Analyse both the original form and the lowercased form.
    keys = M.unionWith (right (M.unionWith min))
        (ana word)
        (ana (T.toLower word))
    right f (x, y) (_, y') = (x, f y y')
    ana = flip F.lookup hd
    follow = fmap (H.entry . fromJust) . H.withKey

-- | Analyse the word using the Morfeusz analyser for contemporary
-- Polish.
anaCont :: T.Text -> [[R.Interp]]
anaCont = map R.interps . head . R.paths . R.analyse False

showText :: [Either Token Other] -> L.Text
showText = L.toLazyText . mconcat . newlineSep . buildText

buildText :: [Either Token Other] -> [L.Builder]
buildText xs = "sent:" :
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
buildHist :: (H.LexEntry, F.Code) -> L.Builder
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
        F.Orig -> "orig"
        F.Copy -> "copy"

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
