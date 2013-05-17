{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | The module provides functions for dictionary-driven analysis
-- of the input text.


module NLP.HistPL.Analyse
(
-- * Tokenization
  Token (..)
, Other (..)
, tokenize
, rmHyphen
-- * Analysis
, anaWord
, mapL
-- * JSON
, JConf (..)
, ShowCont (..)
, defaultJConf
, jsonAna
) where


import           Control.Applicative ((<$>), (<*>), pure)
import           Data.Maybe (fromJust)
import           Data.Monoid (Monoid, mappend, mconcat)
import           Data.Ord (comparing)
import           Data.List (sortBy, intersperse)
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import           Data.Aeson

import qualified NLP.Morfeusz as R
import qualified NLP.HistPL.Lexicon as H


-----------------------------------------------------
-- Basic types
-----------------------------------------------------


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


-----------------------------------------------------
-- Tokenization
-----------------------------------------------------


-- | Remove all instances of the \"-\\n\" string.
rmHyphen :: L.Text -> L.Text
rmHyphen = L.concat . L.splitOn "-\n"


-- | Perform simple tokenization -- spaces and punctuation
-- characters are treated as token ending markers.
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


-----------------------------------------------------
-- Analysis
-----------------------------------------------------
 
 
-- | Map the monadic function over the left elements of the input list.
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


-- | Analyse word using the Morfeusz analyser for contemporary Polish.
anaCont :: T.Text -> [[R.Interp]]
anaCont = map R.interps . head . R.paths . R.analyse False


-----------------------------------------------------
-- JSON
-----------------------------------------------------


-- | When contemporary interpretations should be shown.
data ShowCont
    = NoShowCont    -- ^ Never
    | ShowCont      -- ^ When no historical interpretation
    | ForceShowCont -- ^ Alwas
    deriving (Show, Eq, Ord)


-- | JSON serialization configuration.  Depending on the configuration,
-- different parts of the result will be converted to a JSON format.
data JConf = JConf
    { showCont  :: ShowCont
    -- ^ When to show cont. interpretations.
    , showDefs  :: Bool
    -- ^ Show definitions?
    } deriving (Show, Eq, Ord)


-- | Default JSON serialization configuration.
defaultJConf :: JConf
defaultJConf = JConf
    { showCont  = ShowCont
    , showDefs  = False }


-- | Build JSON value from a list of analysed sentences.
jsonAna :: JConf -> [Either Token Other] -> Value
jsonAna jc = toJSON . map (jsonSent  jc)


-- | JSON representation of a sentence.
jsonSent :: JConf -> Either Token Other -> Value
jsonSent jc = either (jsonTok jc) (jsonOther jc)


-- | JSON represesentation of other segment.
jsonOther :: JConf -> Other -> Value
jsonOther _ (Space _) = toJSON ("space" :: T.Text)
jsonOther _ (Pun t)   = toJSON $ "pun: " `T.append` t


-- | JSON represesentation of a token.
jsonTok :: JConf -> Token -> Value
jsonTok jc tok = object $
    [ "orth" .= orth tok
    , "hist" .= map (jsonHist jc) (hist tok) ] ++
    maybeCont
  where
    maybeCont
        | null (hist tok) && showCont jc /= NoShowCont  = [contElem]
        | showCont jc == ForceShowCont                  = [contElem]
        | otherwise                                     = []
    contElem = "cont" .= jsonCont jc (cont tok)
    

-- | JSON represesentation of a historical interpretation.
jsonHist :: JConf -> (H.LexEntry, H.Code) -> Value
jsonHist jc (entry, code) = object $
    [ "id"   .= jsonID (H.lexID entry, code)
    , "pos"  .= H.pos entry
    , "base" .= H.text (H.lemma entry) ]
    ++ if showDefs jc then [defsElem] else []
  where
    jsonID (id', cd') = toJSON (toJSON id', jsonCode cd')
    jsonCode code' = toJSON $ case code' of
        H.Orig -> "orig" :: T.Text
        H.Both -> "both"
        H.Copy -> "copy"
    defsElem = "defs" .= concatMap H.text (getDefs entry)
    getDefs  = concatMap H.defs . H.senses


-- | JSON represesentation of a contemporary interpretation.
jsonCont :: JConf -> [[R.Interp]] -> Value
jsonCont _ = toJSON . map (map jsonI) where
    jsonI x = object
        [ "base" .= R.base x
        , "msd"  .= R.msd x ]


-----------------------------------------------------
-- Utils
-----------------------------------------------------


(<>) :: Monoid m => m -> m -> m
(<>) = mappend
