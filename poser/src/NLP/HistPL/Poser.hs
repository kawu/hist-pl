{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.HistPL.Poser
( vote
) where


import System.IO (hPutStrLn, stderr, withFile, IOMode(..))
import System.Environment (getArgs)
-- import Codec.Binary.UTF8.String (encodeString, decodeString)
import Data.Char (isSpace)
import Data.Maybe (fromJust, catMaybes)
import Control.Applicative ((*>), (<*), (<$>))
import qualified Control.Applicative as AP
import Control.Monad (forM, forM_, (>=>))
import Data.List (intercalate, sortBy)
import qualified Data.Map as Map
import Data.Function (on)


import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Text.Parsec
import Text.Parsec.Text

import qualified NLP.HistPL.Types as H
import qualified NLP.HistPL.LMF as H
-- import qualified Text.Polh.Parse as H
-- import qualified Data.Polh.IO as H

import qualified NLP.Morfeusz as A

import qualified NLP.HistPL.Poser.Bag as Bag
import qualified NLP.HistPL.Poser.Rules as R
import NLP.HistPL.Poser.Rules (Rule, (=>>), (#>), orthSatisfy, afterAny, msdWith)
import NLP.HistPL.Poser.Entropy


-- | Split definition to a list of equivalents.
-- Remove round brackets and quote characters.
splitDef :: T.Text -> Either ParseError [T.Text]
splitDef def = parse pDef ("definition: " ++ T.unpack def) $ T.strip def
  where
    pDef :: Parser [T.Text]
    pDef = pEq `sepBy` (spaces *> char ',' *> spaces)
    
    pEq :: Parser T.Text
    pEq = T.intercalate " " . filter (not . T.null) <$> pTok `sepBy` spaces
    
    pTok :: Parser T.Text
    pTok  = T.pack <$>
        (   pParens
        <|> pQuoted
        <|> pWord )
    
    pWord = many1 $ satisfy (\c -> not (isSpace c || c == ','))

    -- FIXME: Recursive nesting of brackets/quotes.
    pParens = between (char '(') (char ')') (pNot ')') *> return ""
    pQuoted = between (char '"' <|> char '„') (char '"') (pNot '"')
    pNot x = many $ satisfy (not . (x ==))

-- | Extract POS value from MSD value.
msd2pos :: T.Text -> T.Text
msd2pos = head . T.splitOn ":"

-- Type of any fuction, which takes a word, its iterpretation and
-- returns maybe MSD value with associated weight.  Value might be
-- Nothing, if interpretation has no lemma or msd.
type InterpToMsd = T.Text -> A.Interp -> Maybe (T.Text, Double)

-- | Promote MSD value when base == word.
baseMsd :: InterpToMsd
baseMsd word A.Interp{..} = do
    return $ if word == base
        then (msd, 1.0)
        else (msd, 0.5)

-- | Take only nominative MSD values. 
nomMsd :: InterpToMsd
nomMsd word A.Interp{..} = do
    if T.isInfixOf "nom" msd
        then Just (msd, 1.0)
        else Nothing

-- | Analyse unicode text with Morfeusz.
analyseUnicode :: T.Text -> [[A.Interp]]
analyseUnicode = map A.interps . head . A.paths . A.analyse False

-- | Analyse only the first segment of the unicode text.
analyseHead :: T.Text -> [A.Interp]
analyseHead text =
    head' $ analyseUnicode text
  where
    head' (x:xs) = x
    head' [] = []

-- | Analyse all but the the first segment of the unicode text.
analyseTail :: T.Text -> [A.Interp]
analyseTail text =
    tail' $ analyseUnicode text
  where
    tail' (x:xs) = concat xs
    tail' [] = []

-- | Get multiset of MSDs based on the list of interpretations of
-- the given word.  
getMsds :: InterpToMsd -> T.Text -> (Bag.Bag T.Text)
getMsds interp2msd word =
    -- | Analyse word with Morfeusz.
    let interps = analyseHead word
        msds = catMaybes $ map (interp2msd word) $ interps
    in  Bag.fromList msds

-- | Determine POS multiset from one-word definition.
processSimplePhrase :: T.Text -> Bag.Bag T.Text
processSimplePhrase phrase
    = Bag.fromList
    $ map (\(x, v) -> (msd2pos x, v))
    $ Bag.toList msds
    where msds = getMsds baseMsd phrase

rules :: [Rule]
rules =
    [ "^inf+się"    #> pos "inf" >> orth "się"              =>> "inf"
    , "^pact+się"   #> pos "pact" >> orth "się"             =>> "subst"
    , "^pact+!się"  #> pos "pact" >> orthNot "się"          =>> "pact"
    , "^o+adj"      #> orth "o" >> pos "adj"                =>> "pact"
    , "inf"         #> afterAny $$ pos "inf"                =>> "inf"
    , "subst"       #> afterAny $$ msdWith ["subst", "nom"] =>> "subst"
    , "ger"         #> afterAny $$ msdWith ["ger", "nom"]   =>> "ger" ]
  where
    pos x = msdWith [x]
    orth x = orthSatisfy ((==) x)
    orthNot x = orthSatisfy ((/=) x)
    ($$) = ($)
    infixr 1 $$

-- | Determine POS from multi-word definition.
processComplexPhrase :: T.Text -> IO (Bag.Bag T.Text)
processComplexPhrase phrase = do
    printRule maybeRule
    return $ mkBag maybeRule
  where
    maybeRule = R.findMatch rules $ zip parts msds
    parts = T.splitOn " " phrase
    msds = map msdsOn parts
    msdsOn x = map A.msd $ analyseHead x
    mkBag (Just rule) = Bag.fromList [(R.action rule, 1.0)]
    mkBag Nothing = Bag.empty

    printRule maybeRule = case maybeRule of
        Just rule -> do
            putStr "[" >> T.putStr (R.name rule) >> putStr "]"
            putStr " => "
            T.putStrLn phrase
        Nothing -> do
            putStr "[none]"
            putStr " => "
            T.putStrLn phrase

-- | Determine POS multiset from word.
phrasePos :: T.Text -> IO (Bag.Bag T.Text)
phrasePos phrase = do
    if length (T.splitOn " " phrase) > 1
        then processComplexPhrase phrase
        else return $ processSimplePhrase phrase

-- | Remove malformed definitions.
pruneDefs :: [T.Text] -> IO [T.Text]
pruneDefs defs =
    concat . catMaybes <$> mapM processError (map splitDef defs)
  where
    processError (Left err) = hPutStrLn stderr (show err) >> return Nothing
    processError (Right x) = return $ Just x

-- | Determine POS multiset from a list of definitions as
-- present in the XML file.
phrasesPos :: [T.Text] -> IO (Bag.Bag T.Text)
phrasesPos xs = do
    posBags <- mapM phrasePos xs
    return $ Bag.unions posBags

-- data LexInfo = LexInfo
--     { lexId :: T.Text
--     , lexForms :: [T.Text]
--     , lexDefs  :: [T.Text]
--     , lexPosBag :: Bag.Bag T.Text }

getPosBag :: H.LexEntry -> IO (Bag.Bag T.Text)
getPosBag lexEntry = do
    defs <- pruneDefs $ H.text =<< H.defs =<< H.senses lexEntry
    let forms = (H.text =<< H.forms lexEntry) ++ H.text (H.lemma lexEntry)
    phrasesPos $ defs ++ forms

-- -- | Return bag of (suffix, POS) values for a given lexeme.
-- suffPoss :: LexInfo -> Bag.Bag (T.Text, T.Text)
-- suffPoss info =
--     Bag.fromList $ zip (concatMap suff $ lexForms info) [1, 1 ..]
--   where
--     suff word = case T.splitOn " " word of
--         [x] -> concat
--             [ [ (getSuffix 2 word, pos)
--               , (getSuffix 3 word, pos)
--               , (getSuffix 4 word, pos) ]
--             | pos <- poss ]
--         _ -> []
--     poss = Bag.mostCommon $ lexPosBag info
--     getSuffix k x = T.toLower $ T.reverse $ T.take k $ T.reverse x


-- | Read entries from the first XML file, perform POS assignment on
-- each entry, and write entries to the second file also in a form
-- of an LMF-compatible XML.
vote :: FilePath -> FilePath -> IO ()
vote src dst = withFile dst WriteMode $ \h -> do
    xs <- H.readLMF src
    forM_ xs $ \x -> do
        pos <- Bag.mostCommon <$> getPosBag x
        print (H.text $ H.lemma x, pos)
        L.hPutStrLn h $ H.showLexEntry $ x { H.pos = pos }


-- vote :: FilePath -> L.Text -> IO ()
-- vote polh input = do
--     setEncoding utf8	            -- ^ Set Morfeusz encoding
--     let entries = H.parsePolh input
--     forM_ entries $ \entry -> do
--         pos <- Bag.mostCommon <$> getPosBag entry
--         print (H.text $ H.lemma entry, pos)
--         setPos polh (H.lexId entry) pos
-- 
-- setPos :: FilePath -> T.Text -> [T.Text] -> IO ()
-- setPos polh lexId pos =
--     let setIt x lex = lex { H.pos = x }
--     in  H.updateLexEntry_ polh (T.unpack lexId) (setIt pos)
-- 
-- main :: IO ()
-- main = do
--     [srdPath, binPath] <- getArgs
--     vote binPath =<< L.readFile srdPath 
