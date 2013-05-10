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
-- * Dictionary
  HistPL
-- ** Key
, Key
, UID
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
import Control.Monad.Trans.Maybe (MaybeT (..))
import System.IO.Unsafe (unsafeInterleaveIO)
import System.FilePath ((</>))
import System.Directory ( getDirectoryContents, createDirectoryIfMissing
                        , createDirectory, doesDirectoryExist )
import Data.List (mapAccumL)
import Data.Binary (encodeFile, decodeFile)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.DAWG.Dynamic as DD

import qualified NLP.HistPL.Dict as D
import           NLP.HistPL.Types
import qualified NLP.HistPL.Util as Util


{- $modules
    "NLP.HistPL.Types" module exports hierarchy of data types
    stored in the binary dictionary.
-}


-- | Path to entries in the binary dictionary.
entryDir :: String
entryDir = "entries"


-- | Path to key map in the binary dictionary.
formMapFile :: String
formMapFile = "forms.bin"


-- | A dictionary key which uniquely identifies the lexical entry.
type Key = D.Key UID


-- -- | A dictionary key which uniquely identifies the lexical entry.
-- data Key = Key {
--     -- | First form (presumably lemma) of the lexical entry.
--       keyForm   :: T.Text
--     -- | Unique identifier among lexical entries with the same 'keyForm'.
--     , keyUid    :: UID }
--     deriving (Show, Eq, Ord)


-- | A unique identifier among entries with the same `keyForm`.
type UID = Int


-- | Form representing the lexical entry.
proxy :: LexEntry -> T.Text
proxy entry = case Util.allForms entry of
    (x:_)   -> x
    []      -> error "proxy: entry with no forms"


-- | Convert the key to the path where binary representation of the entry
-- is stored.
showKey :: Key -> String
showKey D.Key{..} = (T.unpack . T.concat) [T.pack (show uid), "-", orth]


-- | Parse the key.
parseKey :: String -> Key
parseKey x =
    let (uid'S, (_:form'S)) = break (=='-') x
    in  D.Key (T.pack form'S) (read uid'S)


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


addKey :: DD.DAWG Char Int -> LexEntry -> (DD.DAWG Char Int, (Key, LexEntry))
addKey m x =
    let main = proxy x
        path = T.unpack main
        num  = maybe 0 id (DD.lookup path m) + 1
        key  = D.Key main num
    in  (DD.insert path num m, (key, x))


addKeys :: [LexEntry] -> [(Key, LexEntry)]
addKeys = snd . mapAccumL addKey DD.empty


mapIO'Lazy :: (a -> IO b) -> [a] -> IO [b]
mapIO'Lazy f (x:xs) = (:) <$> f x <*> unsafeInterleaveIO (mapIO'Lazy f xs)
mapIO'Lazy _ []     = return []


forIO'Lazy :: [a] -> (a -> IO b) -> IO [b]
forIO'Lazy = flip mapIO'Lazy


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
loadEntry :: FilePath -> Key -> IO (Maybe LexEntry)
loadEntry path key = do
    maybeErr $ decodeFile (path </> showKey key)


--------------------------------------------------------
-- Binary interface
--------------------------------------------------------


-- | A binary dictionary holds additional info of type @a@
-- for every entry and additional info of type @b@ for every
-- word form.
data HistPL = HistPL {
    -- | A path to the binary dictionary.
      dictPath  :: FilePath
    -- | A dictionary with lexicon forms.
    , formMap   :: D.Dict UID () ()
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
tryWithKey hpl key = unsafeInterleaveIO $ loadEntry (entryPath hpl) key


-- | Extract lexical entry with a given key.  Raise error if there
-- is no entry with such a key.
withKey :: HistPL -> Key -> IO LexEntry
withKey hpl key = tryWithKey hpl key >>= maybe
    (fail $ "Failed to open entry with the " ++ show key ++ " key") return


-- | Lookup the form in the dictionary.
-- TODO: Apply word to the map in the result.
lookup :: HistPL -> T.Text -> IO [LexEntry]
lookup hpl x = do
    let lexSet = D.lookup x (formMap hpl)
    sequence
        [ withKey hpl key
        | key <- M.keys lexSet ]


--------------------------------------------------------
-- Conversion
--------------------------------------------------------


-- | Construct dictionary from a list of lexical entries and save it in
-- the given directory. 
save :: FilePath -> [LexEntry] -> IO (HistPL)
save path xs = do
    createDirectoryIfMissing True path
    isEmpty <- emptyDirectory path
    when (not isEmpty) $ do
        error $ "save: directory " ++ path ++ " is not empty"
    let lexPath = path </> entryDir
    createDirectory lexPath
    formMap' <- D.fromList . concat <$>
        mapIO'Lazy (saveBin lexPath) (addKeys xs)
    encodeFile (path </> formMapFile) formMap'
    return $ HistPL path formMap'
  where
    saveBin lexPath (key, lexEntry) = do
        saveEntry lexPath key lexEntry
        let D.Key{..} = key
        return
            [ (orth, uid, (), y, ())
            | y <- Util.allForms lexEntry ]


-- | Load all lexical entries in a lazy manner.
load :: HistPL -> IO [LexEntry]
load hpl = do
    keys <- getIndex hpl
    forIO'Lazy keys $ withKey hpl
