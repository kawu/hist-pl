{-# LANGUAGE OverloadedStrings #-}

module NLP.HistPL.Poser.Rules
( Rule (..)
, findMatch
, match
, (=>>)
, (#>)
, orthSatisfy
, afterAny
, msdWith
) where

import Prelude hiding (pred)
import Control.Applicative ((<$>))
import Control.Monad (msum)
import qualified Data.Text as T
import Text.Parsec

-- | Word with a list of MSDs.
type WordMsds = (T.Text, [T.Text])

type Parser = Parsec [WordMsds] () ()

data Rule = Rule
    { name      :: T.Text
    , pred      :: Parser
    , action    :: T.Text }

(=>>) :: Parser -> T.Text -> (Parser, T.Text)
(=>>) = (,)
infixr 0 =>>

(#>) :: T.Text -> (Parser, T.Text) -> Rule
(#>) name (pred, val) = Rule name pred val
infixr 0 #>

match :: Rule -> [WordMsds] -> Maybe Rule
match rule ws = case parse (pred rule) "" ws of
    Left  _ -> Nothing
    Right _ -> Just rule 

findMatch :: [Rule] -> [WordMsds] -> Maybe Rule
findMatch rs ws = msum [match rule ws | rule <- rs]

--------------------------------------------------------------------------
-- MSD primitive parsers
--------------------------------------------------------------------------

msdWith :: [T.Text] -> Parser
msdWith xs =
    ignore $ satisfyT hasMsd
  where
    hasMsd (_, msds)    = any cond msds
    cond msd            = all (`T.isInfixOf` msd) xs


orthSatisfy :: (T.Text -> Bool) -> Parser
orthSatisfy p = ignore $ satisfyT $ \(orth, _) -> p orth

ignore p = p >> return ()
afterAny p = ignore $ manyTill anyT p

--------------------------------------------------------------------------
-- Primitive general combinators
--------------------------------------------------------------------------

satisfyT :: (Show t, Stream s m t) => (t -> Bool) -> ParsecT s u m t
satisfyT p =
    tokenPrim showTok nextPos testTok
  where
    showTok t     = show t
    testTok t     = if p t then Just t else Nothing
    nextPos p t s = incSourceColumn p 1

oneOfT :: (Eq t, Show t, Stream s m t) => [t] -> ParsecT s u m t
oneOfT ts = satisfyT (`elem` ts)

noneOfT :: (Eq t, Show t, Stream s m t) => [t] -> ParsecT s u m t
noneOfT ts = satisfyT (not . (`elem` ts))

anyT :: (Show t, Stream s m t) => ParsecT s u m t
anyT = satisfyT (const True)
