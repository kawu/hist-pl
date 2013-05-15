-- | The module provides a simple embedded domain specific language for
-- defining transliteration rules.  All parsers are case-insensitive
-- by default.


module NLP.HistPL.Transliter
( 
-- * Transliteration
  TrRules (..)
, transliter

-- * Parsers
, Parser
, ciString
, ciChar

-- * Operators
, (#>)
, (>#>)
, (>+>)
, (.|)
, (.|.)
) where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (msum)
import Data.Char (isUpper, toUpper, toLower)
import Text.Parsec hiding (Line)

-- | A parser data type.
type Parser = Parsec String ()

-- | Case insensitive character parser.
ciChar :: Char -> Parser Char
ciChar c = char (toLower c) <|> char (toUpper c)

-- | Case insensitive string parser.
ciString :: String -> Parser String
ciString = sequence . map ciChar

data Shape = Lower
           | Capitalized 
           | Upper

getShape :: String -> Shape
getShape [] = Lower
getShape xs@(x:_)
    | all isUpper xs = Upper
    | isUpper x      = Capitalized
    | otherwise      = Lower 

applyShape :: Shape -> String -> String
applyShape _ [] = []
applyShape Lower xs = map toLower xs
applyShape Upper xs = map toUpper xs
applyShape Capitalized (x:xs) = toUpper x : map toLower xs

-- | A transliteration rule, e.g. (\"abc\" #> \"bcd\") will
-- substitute all \"abc\" (sub)string instances with \"bcd\".
(#>) :: String -> String -> Parser String
(#>) p x = ciString p >#> x

-- | Similar to `#>`, but this function allows to define a custom
-- parser for the string which should be substituted with another
-- string.
(>#>) :: Parser String -> String -> Parser String
(>#>) p x = p >>= \y -> return (applyShape (getShape y) x)

-- | Concatentation of parsers.
(>+>) :: Parser String -> Parser String -> Parser String
(>+>) p p' = (++) <$> p <*> p'

-- | OR parser, i.e. a parser which tries to match the first string argument,
-- and only tries the second one if the first match failed.
(.|) :: String -> String -> Parser String
(.|) x y = try (ciString x) <|> try (ciString y)
-- FIXME: Gdy długość napisu jest <= 1, nie jest potrzebna funkcja try.

-- | Similar to `.|`, but accepts a parser as the first argument.
(.|.) :: Parser String -> String -> Parser String
(.|.) p y = p <|> try (ciString y)
-- FIXME: Gdy długość napisu jest <= 1, nie jest potrzebna funkcja try.

-- | A set of transliteration rules.
data TrRules = TrRules {
    -- | Word-level rule is applied only when it matches the entire word.
      wordRules :: [Parser String]
    -- | Character-level rule is always applied when a match is found.
    , charRules :: [Parser String]
    }

wordParser :: TrRules -> Parser String
wordParser rules = 
    perWord <|> perChar
  where
    perWord = msum $ map (\p -> try $ p <* eof) (wordRules rules)
    perChar = (concat <$> many1 (msum $ map try (charRules rules))) <* eof

parseWord :: TrRules -> String -> String
parseWord rules x =
  case parse (wordParser rules) x x of
    Left err -> error $ "parseWord: " ++ show err
    Right y  -> y

-- | Transliterate the word with the given set of transliteration rules.
transliter :: TrRules -> String -> String
transliter rules x =
  case takeWhile (not . eq) ps of
    [] -> x
    xs -> snd $ last $ xs
  where
    ys = iterate (parseWord rules) x
    ps = zip ys $ tail ys
    eq = uncurry (==)

-- -- Text parsing
-- 
-- removeHyp :: String -> String
-- removeHyp ('‑':'\n':xs) = removeHyp xs
-- removeHyp (x:xs) = x : removeHyp xs
-- removeHyp [] = []
-- 
-- type Text = [Line]
-- type Line = [Seg]
-- data Seg = Orth String
--          | Interp String
--          | Space String
--          deriving (Show)
-- 
-- parseText :: String -> Text
-- parseText text =
--     map parseLine $ lines $ removeHyp $ map toLower text
--   where
--     parseLine x = case parse lineParser x x of
--         Left err -> error $ "parseText: " ++ show err
--         Right y  -> y
--     lineParser = many segParser
--     segParser = (Space <$> spaceParser)
--             <|> (Interp <$> interpParser)
--             <|> (Orth <$> orthParser)
--     spaceParser  = many1 $ satisfy isSpace
--     interpParser = many1 $ satisfy isPunctuation
--     orthParser   = many1 $ satisfy $ \c ->
--         not (isPunctuation c) && not (isSpace c)
-- 
-- unParseText :: Text -> String
-- unParseText =
--     unlines . map unLine
--   where
--     unLine = concatMap unSeg
--     unSeg (Orth x) = transcript x
--     unSeg (Interp x) = x
--     unSeg (Space x) = x
-- 
-- transcriptText :: String -> String
-- transcriptText = unParseText . parseText
