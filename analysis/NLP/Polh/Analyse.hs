{-# LANGUAGE OverloadedStrings #-}

module NLP.Polh.Analyse
( Ana
, Trie
, Token (..)
, Other (..)
, buildTrie
, tokenize
, anaText
, anaWord
, mapL
, showText
) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow (first)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend, mconcat)
import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Data.PoliMorf as Poli
import qualified NLP.Adict.Trie as Trie

import qualified NLP.Polh.Types as H
import qualified NLP.Polh.Binary as H
import qualified NLP.Polh.Util as H

import qualified NLP.Morfeusz as F

-- | Provisional analysis results.
type Ana = M.Map H.Key Poli.RelCode

type Trie = Trie.TrieM Char Ana

data Token = Token {
    -- | Orthographic form.
      orth  :: T.Text
    -- | Historical interpretations.
    , hist  :: [(H.LexEntry, Poli.RelCode)]
    -- | Conteporary interpretations.
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
anaText :: Trie -> T.Text -> H.PolhM [Either Token Other]
anaText trie = mapL (anaWord trie) . tokenize

-- | Map the function over left elements.
mapL :: (Functor m, Monad m) => (a -> m a') -> [Either a b] -> m [Either a' b]
mapL f =
    let g (Left x)  = Left <$> f x
        g (Right y) = return (Right y)
    in  mapM g

-- | Analyse the word.
anaWord :: Trie -> T.Text -> H.PolhM Token
anaWord trie x = do
    _hist <- anaHist trie x
    _cont <- return (anaCont x)
    return $ Token x _hist _cont

-- | Analyse the word with respect to the historical dictionary. 
anaHist :: Trie -> T.Text -> H.PolhM [(H.LexEntry, Poli.RelCode)]
anaHist trie x = sequence
    [ (,) <$> follow key <*> pure relCode
    | (key, relCode) <- ana ]
  where
    ana = case Trie.lookup (mkQ x) trie of
        Nothing -> []
        Just xs -> M.toList xs
    follow = fmap fromJust . H.withKey

-- | Analyse the word using the Morfeusz analyser for contemporary
-- Polish.
anaCont :: T.Text -> [[F.Interp]]
anaCont = map F.interps . head . F.paths . F.analyse False

-- | Parse the LMF historical dictionary, merge it with the PoliMorf
-- and return the resulting trie.
buildTrie
    :: FilePath     -- ^ Path to Polh binary dictionary
    -> FilePath     -- ^ Path to PoliMorf
    -> IO Trie      -- ^ Resulting trie
buildTrie polhPath poliPath = do
    -- TODO: Filter one-word forms?
    baseMap <- Poli.mkBaseMap . filter oneWordEntry
           <$> Poli.readPoliMorf poliPath
    polh <- H.loadPolh polhPath >>= \x -> case x of
        Nothing -> error "buildTrie: not a polh dictionary"
        Just xs -> return $ mkPolh xs
    let polh' = Poli.merge baseMap $ M.fromList polh
    return $ Trie.fromList $ map (first mkQ) (M.assocs polh')
  where
    mkPolh dict =
        [ (x, S.singleton (H.lexKey entry))
        | entry <- dict, x <- H.allForms entry, oneWord x ]

-- | Is it a one-word entry?
oneWordEntry :: Poli.Entry -> Bool
oneWordEntry = oneWord . Poli.form

-- | Is it a one-word text?
oneWord :: T.Text -> Bool
oneWord = (==1) . length . T.words

-- | Make caseless trie search string from the text.
mkQ :: T.Text -> String
mkQ = T.unpack . T.toLower

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
    :  map (indent . buildHist) (hist tok)
    -- ++ concatMap buildCont (cont tok)

buildHead :: Token -> L.Builder
buildHead tok = "word: " <> L.fromText (orth tok)

-- | Build a list of historical interpretations.
buildHist :: (H.LexEntry, Poli.RelCode) -> L.Builder
buildHist (entry, _code)
    =  "hist: "
    <> buildPos <> " "
    <> commaRepr (H.lemma entry)
--     <> " " <> "tags: "
  where
    buildPos = case H.pos entry of
        [] -> "-"
        xs -> mconcat . commaSep . map L.fromText $ xs

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
