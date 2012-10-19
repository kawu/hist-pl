module NLP.Polh.Util
( allForms
, hasForm
, addForm
) where

import qualified Data.Text as T
import NLP.Polh.Types

allForms :: LexEntry -> [T.Text]
allForms lex
    =  text (lemma lex) 
    ++ concatMap text (forms lex)

hasForm :: LexEntry -> T.Text -> Bool
hasForm lex x = x `elem` allForms lex

addForm :: WordForm -> LexEntry -> LexEntry
addForm x lex = lex { forms = (x : forms lex) }
