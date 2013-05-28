module NLP.HistPL.Binary
( save
, load
, tryLoad
, dictIDs
, loadAll
) where


import           Prelude hiding (lookup)
import           Control.Applicative ((<$>))
import           System.FilePath ((</>))
import           Data.Binary (encodeFile, decodeFile)
import qualified Data.Text as T
import qualified Control.Monad.LazyIO as LazyIO

import           NLP.HistPL.Types
import           NLP.HistPL.Binary.Util


-- | Save entry in the given directory (the actual entry path
-- is determined on the basis of the `lexID`).
save :: FilePath -> LexEntry -> IO ()
save path x = encodeFile (path </> T.unpack (lexID x)) x


-- | Lookup entry with a given `lexID`.
load :: FilePath -> T.Text -> IO LexEntry
load path i = decodeFile (path </> T.unpack i)


-- | Lookup entry with a given `lexID`.
tryLoad :: FilePath -> T.Text -> IO (Maybe LexEntry)
tryLoad path i = maybeErr $ load path i


-- | Get a list of entry identifiers stored in the dictionary.
dictIDs :: FilePath -> IO [T.Text]
dictIDs path = map T.pack <$> loadContents path


-- | Load all lexical entries in a lazy manner.
loadAll :: FilePath -> IO [LexEntry]
loadAll path = do
    ids <- dictIDs path
    LazyIO.forM ids $ load path
