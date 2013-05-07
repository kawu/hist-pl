-- | Some utility functions for working with the dictionary.

module NLP.HistPL.Util
( allForms
, hasForm
, addForm
) where

import qualified Data.Text as T
import NLP.HistPL.Types

-- | All format (base form + other forms) of the lexeme.
allForms :: LexEntry -> [T.Text]
allForms lx
    =  text (lemma lx) 
    ++ concatMap text (forms lx)

-- | Does lexeme take the given form? 
hasForm :: LexEntry -> T.Text -> Bool
hasForm lx x = x `elem` allForms lx

-- | Add new word form to the lexeme description.
addForm :: WordForm -> LexEntry -> LexEntry
addForm x lx = lx { forms = (x : forms lx) }
