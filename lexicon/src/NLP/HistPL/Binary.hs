{-# LANGUAGE ScopedTypeVariables #-} 


module NLP.HistPL.Binary
( save
, load
, tryLoad
, getIndex
, loadAll
) where


import           Prelude hiding (lookup)
import           Control.Exception (try, SomeException)
import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           System.FilePath ((</>))
import           System.Directory (getDirectoryContents)
import           Data.Binary (encodeFile, decodeFile)
import qualified Data.Text as T
import qualified Control.Monad.LazyIO as LazyIO

import           NLP.HistPL.Types


-- | Save entry in the given directory (the actual entry path
-- is determined on the basis of the `lexID`).
save :: FilePath -> LexEntry -> IO ()
save path x = encodeFile (path </> T.unpack (lexID x)) x


-- | Lookup entry with a given `lexID`.
load :: FilePath -> T.Text -> IO LexEntry
load path key = decodeFile (path </> T.unpack key)


-- | Lookup entry with a given `lexID`.
tryLoad :: FilePath -> T.Text -> IO (Maybe LexEntry)
tryLoad path = maybeErr . load path


-- | Get a list of entry identifiers stored in the dictionary.
getIndex :: FilePath -> IO [T.Text]
getIndex path = map T.pack <$> loadContents path


-- | Load all lexical entries in a lazy manner.
loadAll :: FilePath -> IO [LexEntry]
loadAll path = do
    keys <- getIndex path
    LazyIO.forM keys $ load path
