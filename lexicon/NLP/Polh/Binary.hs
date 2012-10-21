{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

-- | The module provides functions for working with the binary
-- representation of the historical dictionary of Polish.
-- The dictionary is stored on a disk but we assume that it
-- doesn't change throughtout the program session so that we
-- can provide the pure interface for dictionary reading
-- and searching.

module NLP.Polh.Binary
( savePolh
, loadPolh

, PolhM
, runPolh
, index
, withKey
, lookup
) where

import Prelude hiding (lookup)
import Control.Exception (try, SomeException)
import Control.Monad (when, guard)
import Control.Applicative ((<$>))
import Control.Monad.Reader (ReaderT (..), ask, lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)
import System.FilePath ((</>))
import System.Directory ( getDirectoryContents, createDirectoryIfMissing
                        , createDirectory, doesDirectoryExist )
import Data.Maybe (catMaybes)
import Data.Monoid (mappend, mconcat)
import Data.Binary (encodeFile, decodeFile)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import NLP.Polh.Types
import qualified NLP.Polh.Util as Util

-- | Path to entries in the binary dictionary.
entryDir :: String
entryDir = "entries"

-- | Path to key map in the binary dictionary.
formMapFile :: String
formMapFile = "forms.bin"

-- | A dictionary key (lexical entry ID).
type Key = T.Text

-- | Load the directory contents.
loadContents :: FilePath -> IO [FilePath]
loadContents path = do
    xs <- getDirectoryContents path
    return [x | x <- xs, x /= ".", x /= ".."]

-- | Check if the directory is empty.
emptyDirectory :: FilePath -> IO Bool
emptyDirectory path = null <$> loadContents path

-- | Save the lexical entry on the disk.
saveLexEntry :: FilePath -> LexEntry -> IO ()
saveLexEntry path x = 
    let lexPath = T.unpack . lexId
    in  encodeFile (path </> lexPath x) x

-- | Save the polh dictionary in the empty directory.
savePolh :: FilePath -> Polh -> IO ()
savePolh path xs = do
    createDirectoryIfMissing True path
    isEmpty <- emptyDirectory path
    when (not isEmpty) $ do
        error $ "savePolh: directory " ++ path ++ " is not empty"
    let lexPath = path </> entryDir
    createDirectory lexPath
    formMap' <- mconcat <$> mapM (saveLex lexPath) xs
    encodeFile (path </> formMapFile) formMap'
  where
    saveLex lexPath x = do
        saveLexEntry lexPath x
        return $ lexMap x
    lexMap lexEntry = M.fromListWith mappend
        [ (x, S.singleton key)
        | x <- Util.allForms lexEntry ]
      where
        key = lexId lexEntry

maybeErr :: IO a -> IO (Maybe a)
maybeErr io = do
    r <- try io
    case r of
        Left (_e :: SomeException)  -> return Nothing
        Right x                     -> return (Just x)

maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return
{-# INLINE maybeT #-}

maybeErrT :: IO a -> MaybeT IO a
maybeErrT io = do
    r <- lift (maybeErr io)
    maybeT r

-- | Load lexical entry from disk by its key.
loadLexEntry :: FilePath -> Key -> IO (Maybe LexEntry)
loadLexEntry path key = do
    maybeErr $ decodeFile (path </> T.unpack key)

-- | Binary dictionary data kept in program memory.
data MemData = MemData
    { polhPath  :: FilePath
    , formMap   :: M.Map T.Text (S.Set Key) }

-- | A PolhM monad is a wrapper over the Polish historical
-- dictionary in a binary form.
newtype PolhM a = PolhM (ReaderT MemData IO a)
    deriving (Functor, Monad)

-- | Path to directory with entries.
entryPath :: MemData -> FilePath
entryPath = (</> entryDir) . polhPath

-- | List of dictionary keys.
index :: PolhM [Key]
index = PolhM $ do
    path <- entryPath <$> ask
    map T.pack <$> lift (loadContents path)

-- | Extract lexical entry with the given ID.
withKey :: Key -> PolhM (Maybe LexEntry)
withKey key = PolhM $ do
    path <- entryPath <$> ask
    lift . unsafeInterleaveIO $ loadLexEntry path key

-- | Lookup the form in the dictionary.
lookup :: T.Text -> PolhM [LexEntry]
lookup x = do
    fm <- PolhM $ formMap <$> ask
    keys <- return $ case M.lookup x fm of
        Nothing -> []
        Just xs -> S.toList xs
    catMaybes <$> mapM withKey keys

-- | Execute the Polh monad against the binary Polh representation
-- located in the given directory.  Return Nothing if the directory
-- doesnt' exist or if it doesn't look like a Polh dictionary.
-- We assume that the binary representation doesn't change so we
-- can provide the pure interface.
runPolh :: FilePath -> PolhM a -> Maybe a
runPolh path (PolhM m) = unsafePerformIO . runMaybeT $ do
    formMap' <- maybeErrT $ decodeFile (path </> formMapFile)
    doesExist <- lift $ doesDirectoryExist (path </> entryDir)
    guard doesExist 
    lift $ runReaderT m (MemData path formMap')

-- | Load dictionary from a disk in a lazy manner.  Return 'Nothing'
-- if the path doesn't correspond to a binary representation of the
-- dictionary. 
loadPolh :: FilePath -> Maybe Polh
loadPolh path = runPolh path $ do
    keys <- index
    catMaybes <$> mapM withKey keys

-- We don't provide update functionality since we want only the pure
-- iterface to be visible.  It greatly simplifies the implementation.
--
-- updateLexEntry :: FilePath -> Key -> (LexEntry -> LexEntry) -> IO LexEntry
-- updateLexEntry path lexKey f = do
--     lexEntry <- loadLexEntry path lexKey
--     let lexEntry' = f lexEntry
--     saveLexEntry path lexEntry'
--     return lexEntry'
-- 
-- updateLexEntry_ :: FilePath -> Key -> (LexEntry -> LexEntry) -> IO ()
-- updateLexEntry_ path lexKey f = do
--     lexEntry <- loadLexEntry path lexKey
--     saveLexEntry path (f lexEntry)
