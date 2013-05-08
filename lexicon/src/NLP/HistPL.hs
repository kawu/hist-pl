{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TupleSections #-} 


{-|
    The module provides functions for working with the binary
    representation of the historical dictionary of Polish.

    It is intended to be imported qualified, to avoid name
    clashes with Prelude functions, e.g. 

    > import qualified NLP.HistPL as H
   
    Use `save` and `load` functions to save/load
    the entire dictionary in/from a given directory.  They are
    particularly useful when you want to convert the @LMF@ dictionary
    to a binary format (see "NLP.HistPL.LMF" module).
   
    To search the dictionary, open the binary directory with an
    `open` function.  For example, during a @GHCi@ session:

    >>> hpl <- H.open "srpsdp.bin"
   
    Set the OverloadedStrings extension for convenience:

    >>> :set -XOverloadedStrings
   
    To search the dictionary use the `lookup` function, e.g.

    >>> entries <- H.lookup hpl "dufliwego"

    You can use functions defined in the "NLP.HistPL.Types" module
    to query the entries for a particular feature, e.g.

    >>> map (H.text . H.lemma) entries
    [["dufliwy"]]
-}


module NLP.HistPL
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

-- * Dictionary
, HistPL
-- ** Open
, tryOpen
, open
-- ** Query
, lookup
, getIndex
, tryWithKey
, withKey

-- * Conversion
-- ** Save
, save
-- ** Load
, tryLoad
, load

-- * Modules
-- $modules
, module NLP.HistPL.Types
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
import Data.List (mapAccumL)
import Data.Binary (Binary, get, put, encodeFile, decodeFile)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.DAWG.Dynamic as DD
import qualified Data.DAWG.Static as D

import NLP.HistPL.Types
import qualified NLP.HistPL.Util as Util

{- $modules
    "NLP.HistPL.Types" module exports hierarchy of data types
    stored in the binary dictionary.
-}


-- | Static DAWG version.
type DAWG a  = D.DAWG Char () a


-- | Path to entries in the binary dictionary.
entryDir :: String
entryDir = "entries"


-- | Path to key map in the binary dictionary.
formMapFile :: String
formMapFile = "forms.bin"


-- -- | Entry in the binary dictionary consists of the lexical
-- -- entry and corresponding unique identifier.
-- data BinEntry = BinEntry {
--     -- | Lexical entry.
--       lexEntry  :: LexEntry
--     -- | Unique identifier among lexical entries with the same first form
--     -- (see 'Key' data type).
--     , uid       :: Int }
--     deriving (Show, Eq, Ord)


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


-- -- | Key assigned to the binary entry.
-- binKey :: BinEntry -> Key
-- binKey BinEntry{..} = Key (proxyForm lexEntry) uid


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


-- | Save entry on a disk under the given key.
saveEntry :: FilePath -> Key -> LexEntry -> IO ()
saveEntry path x y = encodeFile (path </> showKey x) y


-- -- | Save the binary entry on the disk.
-- saveBinEntry :: FilePath -> BinEntry -> IO ()
-- saveBinEntry path x =
--     let binPath = showKey . binKey
--     in  encodeFile (path </> binPath x) (lexEntry x)


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


-- | Save the HistPL dictionary in the empty directory.
save :: FilePath -> [LexEntry] -> IO ()
save path xs = do
    createDirectoryIfMissing True path
    isEmpty <- emptyDirectory path
    when (not isEmpty) $ do
        error $ "save: directory " ++ path ++ " is not empty"
    let lexPath = path </> entryDir
    createDirectory lexPath
    formMap' <- D.fromListWith S.union . concat
        <$> mapIO'Lazy (saveBin lexPath) (withUids xs)
    encodeFile (path </> formMapFile) formMap'
  where
    saveBin lexPath x = do
        saveBinEntry lexPath x
        return $ rules x
    rules binEntry =
        [ ( T.unpack x
          , S.singleton (between x key) )
        | x <- Util.allForms (lexEntry binEntry) ]
      where
        key = binKey binEntry


-- | Load dictionary from a disk in a lazy manner.  Raise an error
-- if the path doesn't correspond to a binary representation of the
-- dictionary. 
load :: FilePath -> IO [(Key, LexEntry)]
load path = tryLoad path >>=
    maybe (fail "Failed to open the dictionary") return


-- | Load dictionary from a disk in a lazy manner.  Return 'Nothing'
-- if the path doesn't correspond to a binary representation of the
-- dictionary. 
tryLoad :: FilePath -> IO (Maybe [(Key, LexEntry)])
tryLoad path = runMaybeT $ do
    hpl  <- MaybeT $ tryOpen path
    -- lift $ mapM (withKey hpl) =<< getIndex hpl
    keys <- getIndex hpl
    lift $ sequence
        [
        | 


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
      dictPath  :: FilePath
    -- | A map with lexicon forms.
    , formMap   :: DAWG (S.Set Rule)
    }


-- | Path to directory with entries.
entryPath :: HistPL -> FilePath
entryPath = (</> entryDir) . dictPath


-- | Open the binary dictionary residing in the given directory.
-- Return Nothing if the directory doesn't exist or if it doesn't
-- constitute a dictionary.
tryOpen :: FilePath -> IO (Maybe HistPL)
tryOpen path = runMaybeT $ do
    formMap'  <- maybeErrT $ decodeFile (path </> formMapFile)
    doesExist <- liftIO $ doesDirectoryExist (path </> entryDir)
    guard doesExist 
    return $ HistPL path formMap'


-- | Open the binary dictionary residing in the given directory.
-- Raise an error if the directory doesn't exist or if it doesn't
-- constitute a dictionary.
open :: FilePath -> IO HistPL
open path = tryOpen path >>=
    maybe (fail "Failed to open the dictionary") return


-- | List of dictionary keys.
getIndex :: HistPL -> IO [Key]
getIndex hpl = map parseKey <$> loadContents (entryPath hpl)


-- | Extract lexical entry with a given key.  Return `Nothing` if there
-- is no entry with such a key.
tryWithKey :: HistPL -> Key -> IO (Maybe LexEntry)
tryWithKey hpl key = unsafeInterleaveIO $ loadLexEntry (entryPath hpl) key


-- | Extract lexical entry with a given key.  Raise error if there
-- is no entry with such a key.
withKey :: HistPL -> Key -> IO LexEntry
withKey hpl key = tryWithKey hpl key >>= maybe
    (fail $ "Failed to open entry with the " ++ show key ++ " key") return


-- | Lookup the form in the dictionary.
lookup :: HistPL -> T.Text -> IO [LexEntry]
lookup hpl x = do
    let keys = case D.lookup (T.unpack x) (formMap hpl) of
            Nothing -> []
            Just xs -> map (flip apply x) (S.toList xs)
    mapM (withKey hpl) keys
