{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TupleSections #-} 


-- | The module provides functions for working with the binary
-- representation of the historical dictionary of Polish.
--
-- Use the `savePolh` and `loadPolh` functions to save/load
-- the entire dictionary in/from a given directory.
--
-- To search the dictionary use the interface provided with 
-- the `PH` monad.  For example, to lookup a word in the
-- dictionary use the `lookup` function:
--
-- >>> runPH "srpsdp.bin" $ do
-- >>>     entry <- lookup "abentair"


module NLP.Polh.Binary
(
-- * Entries
  BinEntry (..)
, Key (..)
, proxyForm
, binKey

-- * Rules
, Rule (..)
, between
, apply

-- * Save
, savePolh

-- * Load
, loadPolh

-- * Binary
, HistPL
, openHistPL
, getIndex
, withKey
, lookup
) where


import Prelude hiding (lookup)
import Control.Exception (try, SomeException)
import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad (when, guard)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import System.IO.Unsafe (unsafeInterleaveIO)
import System.FilePath ((</>))
import System.Directory ( getDirectoryContents, createDirectoryIfMissing
                        , createDirectory, doesDirectoryExist )
import Data.Maybe (catMaybes)
import Data.List (mapAccumL)
import Data.Binary (Binary, get, put, encodeFile, decodeFile)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.DAWG.Dynamic as DD
import qualified Data.DAWG.Static as D

import NLP.Polh.Types
import qualified NLP.Polh.Util as Util


-- | Static DAWG version.
type DAWG a  = D.DAWG Char () a


-- | Path to entries in the binary dictionary.
entryDir :: String
entryDir = "entries"


-- | Path to key map in the binary dictionary.
formMapFile :: String
formMapFile = "forms.bin"


-- | Entry in the binary dictionary consists of the lexical
-- entry and corresponding unique identifier.
data BinEntry = BinEntry {
    -- | Lexical entry.
      entry :: LexEntry
    -- | Unique identifier among lexical entries with the same first form
    -- (see 'Key' data type).
    , uid   :: Int }
    deriving (Show, Eq, Ord)


instance Binary BinEntry where
    put BinEntry{..} = put entry >> put uid
    get = BinEntry <$> get <*> get


-- | A dictionary key which uniquely identifies the lexical entry.
data Key = Key {
    -- | First form (presumably lemma) of the lexical entry.
      keyForm   :: T.Text
    -- | Unique identifier among lexical entries with the same 'keyForm'.
    , keyUid    :: Int }
    deriving (Show, Eq, Ord)


-- | Form representing the lexical entry.
proxyForm :: LexEntry -> T.Text
proxyForm entry = case Util.allForms entry of
    (x:_)   -> x
    []      -> error "proxyForm: entry with no forms"


-- | Key assigned to the binary entry.
binKey :: BinEntry -> Key
binKey BinEntry{..} = Key (proxyForm entry) uid


-- | Convert the key to the path where binary representation of the entry
-- is stored.
showKey :: Key -> String
showKey Key{..} = (T.unpack . T.concat) [T.pack (show keyUid), "-", keyForm]


-- | Parse the key.
parseKey :: String -> Key
parseKey x =
    let (uid'S, (_:form'S)) = break (=='-') x
    in  Key (T.pack form'S) (read uid'S)


-- | Load the directory contents.
loadContents :: FilePath -> IO [FilePath]
loadContents path = do
    xs <- getDirectoryContents path
    return [x | x <- xs, x /= ".", x /= ".."]


-- | Check if the directory is empty.
emptyDirectory :: FilePath -> IO Bool
emptyDirectory path = null <$> loadContents path


-- | Save the binary entry on the disk.
saveLexEntry :: FilePath -> BinEntry -> IO ()
saveLexEntry path x =
    let binPath = showKey . binKey
    in  encodeFile (path </> binPath x) x


withUid :: DD.DAWG Char Int -> LexEntry -> (DD.DAWG Char Int, BinEntry)
withUid m x =
    let path = T.unpack (proxyForm x)
        num  = maybe 0 id (DD.lookup path m) + 1
    in  (DD.insert path num m, BinEntry x num)


withUids :: [LexEntry] -> [BinEntry]
withUids = snd . mapAccumL withUid DD.empty


mapIO'Lazy :: (a -> IO b) -> [a] -> IO [b]
mapIO'Lazy f (x:xs) = (:) <$> f x <*> unsafeInterleaveIO (mapIO'Lazy f xs)
mapIO'Lazy _ []     = return []


-- | Save the polh dictionary in the empty directory.
savePolh :: FilePath -> Polh -> IO ()
savePolh path xs = do
    createDirectoryIfMissing True path
    isEmpty <- emptyDirectory path
    when (not isEmpty) $ do
        error $ "savePolh: directory " ++ path ++ " is not empty"
    let lexPath = path </> entryDir
    createDirectory lexPath
    formMap' <- D.fromListWith S.union . concat
        <$> mapIO'Lazy (saveLex lexPath) (withUids xs)
    encodeFile (path </> formMapFile) formMap'
  where
    saveLex lexPath x = do
        saveLexEntry lexPath x
        return $ rules x
    rules binEntry =
        [ ( T.unpack x
          , S.singleton (between x key) )
        | x <- Util.allForms (entry binEntry) ]
      where
        key = binKey binEntry


-- | Load dictionary from a disk in a lazy manner.  Return 'Nothing'
-- if the path doesn't correspond to a binary representation of the
-- dictionary. 
loadPolh :: FilePath -> IO (Maybe [BinEntry])
loadPolh path = runMaybeT $ do
    hpl  <- MaybeT $ openHistPL path
    lift $ do
        keys <- getIndex hpl
        catMaybes <$> mapM (withKey hpl) keys


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
loadLexEntry :: FilePath -> Key -> IO (Maybe BinEntry)
loadLexEntry path key = do
    maybeErr $ decodeFile (path </> showKey key)


--------------------------------------------------------
-- Rules
--------------------------------------------------------


-- | A rule for translating a form into a binary dictionary key.
data Rule = Rule {
    -- | Number of characters to cut from the end of the form.
      cut       :: !Int
    -- | A suffix to paste.
    , suffix    :: !T.Text
    -- | Unique identifier of the entry.
    , ruleUid   :: !Int }
    deriving (Show, Eq, Ord)


instance Binary Rule where
    put Rule{..} = put cut >> put suffix >> put ruleUid
    get = Rule <$> get <*> get <*> get


-- | Apply the rule.
apply :: Rule -> T.Text -> Key
apply r x =
    let y = T.take (T.length x - cut r) x `T.append` suffix r
    in  Key y (ruleUid r)


-- | Make a rule which translates between the string and the key.
between :: T.Text -> Key -> Rule
between source dest =
    let k = lcp source (keyForm dest)
    in  Rule (T.length source - k) (T.drop k (keyForm dest)) (keyUid dest)
  where
    lcp a b = case T.commonPrefixes a b of
        Just (c, _, _)  -> T.length c
        Nothing         -> 0


--------------------------------------------------------
-- Binary interface
--------------------------------------------------------


-- | A binary dictionary handle.
data HistPL = HistPL {
    -- | A path to the binary dictionary.
      polhPath  :: FilePath
    -- | A map with lexicon forms.
    , formMap   :: DAWG (S.Set Rule)
    }


-- | Path to directory with entries.
entryPath :: HistPL -> FilePath
entryPath = (</> entryDir) . polhPath


-- | Open the binary dictionary residing in the given directory.
-- Return Nothing if the directory doesn't exist or if it doesn't
-- look like a PH dictionary.
openHistPL :: FilePath -> IO (Maybe HistPL)
openHistPL path = runMaybeT $ do
    formMap'    <- maybeErrT $ decodeFile (path </> formMapFile)
    doesExist   <- liftIO $ doesDirectoryExist (path </> entryDir)
    guard doesExist 
    return $ HistPL path formMap'


-- | List of dictionary keys.
getIndex :: HistPL -> IO [Key]
getIndex hpl = map parseKey <$> loadContents (entryPath hpl)


-- | Extract lexical entry with a given key.
withKey :: HistPL -> Key -> IO (Maybe BinEntry)
withKey hpl key =  unsafeInterleaveIO $ loadLexEntry (entryPath hpl) key


-- | Lookup the form in the dictionary.
lookup :: HistPL -> T.Text -> IO [BinEntry]
lookup hpl x = do
    let keys = case D.lookup (T.unpack x) (formMap hpl) of
            Nothing -> []
            Just xs -> map (flip apply x) (S.toList xs)
    catMaybes <$> mapM (withKey hpl) keys
