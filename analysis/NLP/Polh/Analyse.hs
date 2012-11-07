module NLP.Polh.Analyse
( LexId
, Ana
, Trie
, Token (..)
, Other (..)
, buildTrie
, tokenize
, anaText
, anaWord
, mapL
) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow (first)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.PoliMorf as Poli
import qualified NLP.Adict.Trie as Trie

import qualified NLP.Polh.Types as H
import qualified NLP.Polh.Binary as H
import qualified NLP.Polh.Util as H

import qualified NLP.Morfeusz as F

-- | Lexeme identifier.
type LexId = T.Text

-- | Provisional analysis results.
type Ana = M.Map LexId Poli.RelCode

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
-- FIXME: There is no guarantee that lexId is equall to key in
-- binary dictionary.
anaHist :: Trie -> T.Text -> H.PolhM [(H.LexEntry, Poli.RelCode)]
anaHist trie x = sequence
    [ (,) <$> follow lexId <*> pure relCode
    | (lexId, relCode) <- ana ]
  where
    ana = case Trie.lookup (mkKey x) trie of
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
    return $ Trie.fromList $ map (first mkKey) (M.assocs polh')
  where
    mkPolh dict =
        [ (x, S.singleton (H.lexId lx))
        | lx <- dict, x <- H.allForms lx, oneWord x ]

-- | Is it a one-word entry?
oneWordEntry :: Poli.Entry -> Bool
oneWordEntry = oneWord . Poli.form

-- | Is it a one-word text?
oneWord :: T.Text -> Bool
oneWord = (==1) . length . T.words

-- | Make caseless trie key from the text.
mkKey :: T.Text -> String
mkKey = T.unpack . T.toLower
