-- | The module provides types and functions for manipulating
-- a (doc <-> ID) correspondence, i.e. a correspondence between
-- documents (from a directory tree) and their unique identifiers.


-- | A document is represented by its file path.
type Doc = FilePath


-- | An abbreviated identifier.
data ID = ID
     { name :: String
     -- ^ Name of the document file.
     , uid  :: String
     -- ^ 
       


-- | A mapping between docs and corresponding identifiers.
type Corresp = M.Map Doc ID


 = do
  [dir] <- getArgs
  
