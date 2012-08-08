module Data.Polh.Util
( allForms
, hasForm
, addForm
) where

import qualified Data.Text as T
import Data.Lens.Common

import Data.Polh.Types

allForms :: LexEntry -> [T.Text]
allForms lex
    =  text (lemma lex) 
    ++ concatMap text (forms lex)

hasForm :: LexEntry -> T.Text -> Bool
hasForm lex x = x `elem` allForms lex

formsL :: Lens LexEntry [WordForm]
formsL = lens forms (\xs lex -> lex {forms = xs})

addForm :: WordForm -> LexEntry -> LexEntry
addForm x = formsL ^%= (x:)    
