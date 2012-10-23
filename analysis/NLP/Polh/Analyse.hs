module NLP.Polh.Analyse
( Token (..)
, Ana (..)
, Other
, buildTrie
, tokenize
, anaSent
, anaWord
, mapL
) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.PoliMorf as Poli
import qualified NLP.Adict.Trie as Trie

import qualified NLP.Polh.Types as H
import qualified NLP.Polh.LMF as H
import qualified NLP.Polh.Util as H

-- | Is it a historical word?
type LexId = T.Text

type Trie = Trie.TrieM Char (Maybe LexId)

data Token = Token
    { orth  :: T.Text
    , ana   :: Ana }
    deriving (Show)

-- | Analysis result.
data Ana
    -- | Historical word.
    = Hist LexId  -- [H.LexEntry]
    -- | Contemporary word.
    | Cont  -- [M.Interp]
    -- | Unknown word.
    | Unk
    deriving (Show)

data Other
    -- | Punctuation.
    = Pun T.Text
    -- | Space
    | Space T.Text

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

anaSent :: Trie -> T.Text -> [Either Token Other]
anaSent trie = mapL (anaWord trie) . tokenize

anaWord :: Trie -> T.Text -> Token
anaWord trie x = Token x $ case Trie.lookup (T.unpack x) trie of
    Just (Just i)   -> Hist i
    Just Nothing    -> Cont
    Nothing         -> Unk

-- | Map the function over left elements.
mapL :: (a -> a') -> [Either a b] -> [Either a' b]
mapL f =
    let g (Left x)  = Left (f x)
        g (Right y) = Right y
    in  map g

-- | Parse the LMF historical dictionary, merge it with the PoliMorf
-- and return the resulting trie.
buildTrie
    :: FilePath     -- ^ Path to LMF
    -> FilePath     -- ^ Path to PoliMorf
    -> IO Trie      -- ^ Resulting trie
buildTrie lmfPath poliPath = do
    -- TODO: Filter one-word forms.
    polh <- mkPolh <$> H.readPolh lmfPath
    baseMap <- Poli.mkBaseMap <$> Poli.readPoliMorf poliPath
    let polh' = Poli.merge baseMap $ M.fromList polh
        trie = Trie.fromList $ map (first T.unpack) (M.assocs polh')
    return $ fmap rmCode trie
  where
    mkPolh xs =
        [ (x, H.lexId lx)
        | lx <- xs, x <- H.allForms lx ]
    rmCode = fmap (fmap fst)
