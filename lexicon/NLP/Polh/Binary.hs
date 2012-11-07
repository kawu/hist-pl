{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

-- | The module provides functions for working with the binary
-- representation of the historical dictionary of Polish.

module NLP.Polh.Binary
( Key
, lexKey

, savePolh
, loadPolh

, PolhT
, runPolhT
, PolhM
, runPolh
, index
, withKey
, lookup
) where

import Prelude hiding (lookup)
import Control.Exception (try, SomeException)
import Control.Monad (when, guard)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative (Applicative, (<$>))
import Control.Monad.Reader (ReaderT (..), ask, lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import System.IO.Unsafe (unsafeInterleaveIO)
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

-- | A dictionary key.
type Key = T.Text

-- | Key assigned to the lexical entry.
lexKey :: LexEntry -> Key
lexKey = lexId

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
        [ (x, S.singleton (lexKey lexEntry))
        | x <- Util.allForms lexEntry ]

maybeErr :: MonadIO m => IO a -> m (Maybe a)
maybeErr io = do
    r <- liftIO (try io)
    case r of
        Left (_e :: SomeException)  -> return Nothing
        Right x                     -> return (Just x)

maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return
{-# INLINE maybeT #-}

maybeErrT :: MonadIO m => IO a -> MaybeT m a
maybeErrT io = do
    r <- liftIO (maybeErr io)
    maybeT r

-- | Load lexical entry from disk by its key.
loadLexEntry :: FilePath -> Key -> IO (Maybe LexEntry)
loadLexEntry path key = do
    maybeErr $ decodeFile (path </> T.unpack key)

-- | Binary dictionary data kept in program memory.
data MemData = MemData
    { polhPath  :: FilePath
    , formMap   :: M.Map T.Text (S.Set Key) }

-- | A Polh monad transformer.
newtype PolhT m a = PolhT (ReaderT MemData m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | A Polh monad is a Polh monad transformer over the hidden IO monad.
type PolhM a = PolhT IO a

-- | Path to directory with entries.
entryPath :: MemData -> FilePath
entryPath = (</> entryDir) . polhPath

-- | List of dictionary keys.
index :: (Applicative m, MonadIO m) => PolhT m [Key]
index = PolhT $ do
    path <- entryPath <$> ask
    map T.pack <$> liftIO (loadContents path)

-- | Extract lexical entry with the given ID.
withKey :: (Applicative m, MonadIO m) => Key -> PolhT m (Maybe LexEntry)
withKey key = PolhT $ do
    path <- entryPath <$> ask
    liftIO . unsafeInterleaveIO $ loadLexEntry path key

-- | Lookup the form in the dictionary.
lookup :: (Applicative m, MonadIO m) => T.Text -> PolhT m [LexEntry]
lookup x = do
    fm <- PolhT $ formMap <$> ask
    keys <- return $ case M.lookup x fm of
        Nothing -> []
        Just xs -> S.toList xs
    catMaybes <$> mapM withKey keys

-- | Execute the Polh monad against the binary Polh representation
-- located in the given directory.  Return Nothing if the directory
-- doesnt' exist or if it doesn't look like a Polh dictionary.
runPolh :: FilePath -> PolhM a -> IO (Maybe a)
runPolh path polh = runPolhT path polh

-- | Execute the Polh monad transformer against the binary Polh representation
-- located in the given directory.  Return Nothing if the directory doesnt'
-- exist or if it doesn't look like a Polh dictionary.
runPolhT :: MonadIO m => FilePath -> PolhT m a -> m (Maybe a)
runPolhT path (PolhT r) = runMaybeT $ do
    formMap' <- maybeErrT $ decodeFile (path </> formMapFile)
    doesExist <- liftIO $ doesDirectoryExist (path </> entryDir)
    guard doesExist 
    lift $ runReaderT r (MemData path formMap')

-- | Load dictionary from a disk in a lazy manner.  Return 'Nothing'
-- if the path doesn't correspond to a binary representation of the
-- dictionary. 
loadPolh :: FilePath -> IO (Maybe Polh)
loadPolh path = runPolhT path $ do
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
