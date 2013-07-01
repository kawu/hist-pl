-- | The program takes a directory with a collection of documents
-- and prints a list of document paths with corresponding
-- unique identifiers.

import System.Environment (getArgs)

main = do
  [dir] <- getArgs
  
