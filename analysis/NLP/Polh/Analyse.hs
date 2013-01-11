{-# LANGUAGE OverloadedStrings #-}

module NLP.Polh.Analyse
( DAWG
, Token (..)
, Other (..)
, buildDAWG
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
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Data.DAWG.Static as DAWG
import qualified Data.PoliMorf as Poli

import qualified NLP.Polh.Types as H
import qualified NLP.Polh.Binary as H
import qualified NLP.Polh.Util as H
import qualified NLP.Polh.Fusion as Fusion

import qualified NLP.Morfeusz as F

type DAWG = DAWG.DAWG Char () (M.Map H.Rule Fusion.RelCode)

data Token = Token {
    -- | Orthographic form.
      orth  :: T.Text
    -- | Historical interpretations.
    , hist  :: [(H.LexEntry, Fusion.RelCode)]
    -- | Contemporary interpretations.
    , cont  :: [[F.Interp]] }
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
anaText :: DAWG -> T.Text -> H.PolhM [Either Token Other]
anaText dawg = mapL (anaWord dawg) . tokenize

-- | Map the monadic function over left elements.
mapL :: (Functor m, Monad m) => (a -> m a') -> [Either a b] -> m [Either a' b]
mapL f =
    let g (Left x)  = Left <$> f x
        g (Right y) = return (Right y)
    in  mapM g

-- | Analyse the word.
anaWord :: DAWG -> T.Text -> H.PolhM Token
anaWord dawg x = do
    _hist <- anaHist dawg x
    _cont <- return (anaCont x)
    return $ Token x _hist _cont

-- | Analyse the word with respect to the historical dictionary. 
anaHist :: DAWG -> T.Text -> H.PolhM [(H.LexEntry, Fusion.RelCode)]
anaHist dawg word = sequence
    [ (,) <$> follow key <*> pure relCode
    | (key, relCode) <- M.toList keys ]
  where
    -- Analyse both the original form and the lowercased form.
    keys = M.unionWith min
        (keysOn word)
        (keysOn (T.toLower word))
    keysOn x = M.fromList
        [ (H.apply rule x, relCode)
        | (rule, relCode) <- ana x ]
    ana x = case DAWG.lookup (T.unpack x) dawg of
        Nothing -> []
        Just m  -> M.toList m
    follow = fmap (H.entry . fromJust) . H.withKey

-- | Analyse the word using the Morfeusz analyser for contemporary
-- Polish.
anaCont :: T.Text -> [[F.Interp]]
anaCont = map F.interps . head . F.paths . F.analyse False

-- | Parse the LMF historical dictionary, merge it with PoliMorf
-- and return the resulting DAWG.
buildDAWG
    :: FilePath     -- ^ Path to Polh binary dictionary
    -> FilePath     -- ^ Path to PoliMorf
    -> IO DAWG      -- ^ Resulting DAWG
buildDAWG polhPath poliPath = do
    baseMap <- Fusion.mkBaseMap . filter Poli.atomic
           <$> Poli.readPoliMorf poliPath
    polh <- H.loadPolh polhPath >>= \x -> case x of
        Nothing -> error "buildDAWG: not a polh dictionary"
        Just xs -> return $ mkPolh xs
    let polh' = Fusion.mergeWith join baseMap polh
    return . DAWG.fromList $ DAWG.assocs polh'
  where
    mkPolh dict = DAWG.fromListWith S.union
        [ (T.unpack x, S.singleton (between x binEntry))
        | binEntry <- dict
        , x <- H.allForms (H.entry binEntry)
        , oneWord x ]
    between x entry = H.between x (H.binKey entry)
    -- Determine rule which translates x'S to the same key as the
    -- y'rule translates y'S to.
    join x'S y'S y'rule =
        H.between x k
      where
        x = T.pack x'S
        y = T.pack y'S
        k = H.apply y'rule y

-- | Is it a one-word text?
oneWord :: T.Text -> Bool
oneWord = (==1) . length . T.words

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
buildHist :: (H.LexEntry, Fusion.RelCode) -> L.Builder
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
        Fusion.Exact  -> "exact"
        Fusion.ByBase -> "base"
        Fusion.ByForm -> "form"

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
