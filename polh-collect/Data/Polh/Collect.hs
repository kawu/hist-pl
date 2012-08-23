{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Polh.Collect
( collect
) where

import System.IO
import System.Environment (getArgs)
import Data.Monoid (mappend)
import Data.Char (toLower, toUpper, isLower, isPunctuation)
import Data.Ord (comparing)
import Control.Applicative ((<$>))
import Control.Monad (forM_, when, guard)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import Control.Monad.Trans (lift)
import Control.Exception (bracket_)
import Data.List (intercalate, minimumBy, sortBy, mapAccumL, tails)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V

import Data.PoliMorf (RelCode(..))
import Data.DAWG.Array (DAWGArray, size)
import Data.Adict.CostOrd
import Data.Adict.ShortestPath (search)

import qualified Data.Token as Tok
import qualified Text.Tokenize.Simple.String as Tok
import qualified Text.Tokenize.Util.String as Tok

import Text.Transcript (transcript)
import Text.Transcript.Impact (rules)

import qualified Data.Polh.Types as Polh
import qualified Data.Polh.IO as Polh
import qualified Data.Polh.Util as Polh

type ID = T.Text
type HistDict = DAWGArray (Maybe (Maybe (ID, RelCode)))

-- | Threshold base. See cost definition for details of how is it used.
thBase = 0.1

-- decodeDict :: FilePath -> IO HistDict
-- decodeDict = decodeFile

-- | Weight (cost) of changing x character to (toLower x) character
-- and vice versa.
lowerEqWeight :: Double
lowerEqWeight = 0.1

cost :: Int -> CostOrd
cost n =

    CostOrd insert delete subst posMod

  where

    insert = [Filter (const True) 1]

    delete x
        | isPunctuation x = 0.5
        | otherwise         = 1

    subst x =
        [ Filter eq 0
        , Filter lowerEq lowerEqWeight ]
        ++ sdGroups subDsc
        -- | TODO: Shortest path is implemented in such a way, that
        -- we can have here [Filter (const True) 1] as the last "rule".
        -- It is ensured, that all graph nodes will be visited at most once.
        -- Thus, we can simplify SubDsc data definition in Adict.CostOrd.
        ++ [Filter (const True) 1]
      where
        subDsc = subDscOn x subDscMap
        eq = (x==)
        lowerEq = (toLower x ==) . toLower

    posMod k
        | k <= n_2  = 1
        | otherwise = (n - k + 1) ./. (n - n_2 + 1)
    x ./. y = fromIntegral x / fromIntegral y
    n_2 = (n + 1) `div` 2

subDscMap :: SubDscMap
subDscMap = mkSDM $ concatMap (uncurry mkGroup)
    [ ("aą", 0.25)
    , ("eę", 0.25)
    , ("oó", 0.25)
    , ("cć", 0.25)
    , ("lł", 0.25)
    , ("nń", 0.25)
    , ("sś", 0.25)
    , ("zźż", 0.25)
    , ("yij", 0.5)
    , ("sz", 0.5)
    , ("kc", 0.75) ]
  where
    mkGroup xs w = concat
        [ [ (x, y, w)
          , (toUpper x, toUpper y, w)
          , (toUpper x, y, w + lowerEqWeight)
          , (x, toUpper y, w + lowerEqWeight) ]
        | x <- xs , y <- xs , x /= y ]

collect
    :: FilePath -- ^ Path to a binary polh lexicon
    -> HistDict -- ^ Poli-hist DAWG
    -> T.Text   -- ^ Input identifier (will be stored in binary lexicon)
    -> Bool     -- ^ Should the input text be transcripted
    -> String   -- ^ Plain text, from which new forms will be collected
    -> IO ()    -- ^ New forms are stored in a binary lexicon
collect polh poliHist src doTrans input = do
    -- let toks = mapMaybe Tok.unOrth (Tok.tokenize input)
    let cxts = mapMaybe unOrth . contexts 5 . Tok.tokenize $ input
            where unOrth (l, x, r) = (l, , r) <$> Tok.unOrth x
    -- forM_ toks $ \tok -> runMaybeT $ do
    forM_ cxts $ \(left, tok, right) -> runMaybeT $ do
        let tokTr = if doTrans
                then transcript rules tok
                else tok
        (path, info, w)  <- maybe (doSearch poliHist tokTr)
        (lexId, relCode) <- maybe info
        guard (w > 0)
        guard (map toLower tokTr /= map toLower path)
        lift $ putStr $ "[" ++ T.unpack lexId ++ "] "
        if doTrans 
            then lift $ putStr $ tok ++ " => " ++ tokTr ++ " => "
            else lift $ putStr $ tok ++ " => "
        lift $ putStrLn $ path ++ " (" ++ show w ++ ", " ++ show relCode ++ ")"
        lift $ pushWord polh src lexId (norm path tokTr)
        -- lift $ pushContext lexId ...
  where
    norm entry = T.pack . normWord entry
    doSearch poliHist x = 
        let n = length x
        in  search (cost n) (threshold thBase n) (V.fromList x) poliHist
    threshold base n
        | n > 10    = base * 10
        | otherwise = base * fromIntegral n
    maybe = MaybeT . return

-- | Make string from a token list. All space-like characters (including
-- newline) are changed to a plain space.
tokStr :: [Tok.Token String] -> String
tokStr =
    Tok.unTokenize . map toSpace
  where
    toSpace (Tok.Space _) = Tok.Space " "
    toSpace x = x

-- | Given right and left context size, input [a] list, return a list
-- of elements with right and left contexts of given size.
contexts :: Int -> [a] -> [([a], a, [a])]
contexts k =
    let f sx (x:xs) = (take k (x:sx), (reverse sx, x, take k xs))
    in  snd . mapAccumL f [] . init . tails

-- flattenContext :: ([a], a, [a]) -> [a]
-- flattenContext (xs, x, ys) = xs ++ (x:ys)

-- | "Normalize" word with respect to dictionary entry.
normWord :: String -> String -> String
normWord entry word =
    if isLower (head entry)
        then map toLower word
        else toUpper (head word) : map toLower (tail word)

-- | Send word to basex server as a form of a given entry with given
-- lexical entry identifier.
pushWord :: FilePath -> T.Text -> T.Text -> T.Text -> IO ()
pushWord polh src lexId word = do
    let repr = Polh.Repr
          { Polh.writtenForm = word
          , Polh.language    = "polh"
          , Polh.sourceID    = src `T.append` "#automatic" }
    let form = Polh.WordForm [repr]
    Polh.updateLexEntry_ polh (T.unpack lexId) (addFormSafe form)
  where
    addFormSafe form lex
        | lex `Polh.hasForm` word = lex
        | otherwise = Polh.addForm form lex

-- pushContext :: FilePath -> T.Text -> T.Text -> T.Text
-- pushContext polh src lexId context = do
--     let repr = Polh.Repr
--           { Polh.writtenForm = context
--           , Polh.language    = "polh"
--           , Polh.sourceID    = src `T.append` "#automatic" }
--     let wrapContext = Polh.WordForm [repr]
--     Polh.updateLexEntry_ polh (T.unpack lexId) (addContextSafe wrapContext)
--   where
--     addContextSafe context lex
--         | (length . concatMap Polh.cxts . Polh.senses $ lex = lex
--         | lex `Polh.hasForm` word = lex
--         | otherwise = Polh.addForm form lex
