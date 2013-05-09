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
, loadInfo

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
import Data.Binary (Binary, encodeFile, decodeFile)
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


addKey
    :: DD.DAWG Char Int -> LexEntry
    -> (DD.DAWG Char Int, (Key, LexEntry))
addKey m x =
    let main = proxyForm x
        path = T.unpack main
        num  = maybe 0 id (DD.lookup path m) + 1
        key  = Key main num
    in  (DD.insert path num m, (key, x) )


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


-- | A unique identifier among entries with the same proxy form.
type UID = Int


-- | A binary dictionary holds additional info of type @a@
-- for every entry and additional info of type @b@ for every
-- word form.
data HistPL a b = HistPL {
    -- | A path to the binary dictionary.
      dictPath  :: FilePath
    -- | A map with lexicon forms.
    , formMap   :: D.Dict UID a b
    }


-- | Path to directory with entries.
entryPath :: HistPL a b -> FilePath
entryPath = (</> entryDir) . dictPath


-- | Open the binary dictionary residing in the given directory.
-- Return Nothing if the directory doesn't exist or if it doesn't
-- constitute a dictionary.
tryOpen :: (Binary a, Binary b) => FilePath -> IO (Maybe (HistPL a b))
tryOpen path = runMaybeT $ do
    formMap'  <- maybeErrT $ decodeFile (path </> formMapFile)
    doesExist <- liftIO $ doesDirectoryExist (path </> entryDir)
    guard doesExist 
    return $ HistPL path formMap'


-- | Open the binary dictionary residing in the given directory.
-- Raise an error if the directory doesn't exist or if it doesn't
-- constitute a dictionary.
open :: (Binary a, Binary b) => FilePath -> IO (HistPL a b)
open path = tryOpen path >>=
    maybe (fail "Failed to open the dictionary") return


-- | List of dictionary keys.
getIndex :: HistPL a b -> IO [Key]
getIndex hpl = map parseKey <$> loadContents (entryPath hpl)


-- | Extract lexical entry with a given key.  Return `Nothing` if there
-- is no entry with such a key.
tryWithKey :: HistPL a b -> Key -> IO (Maybe LexEntry)
tryWithKey hpl key = unsafeInterleaveIO $ loadEntry (entryPath hpl) key


-- | Extract lexical entry with a given key.  Raise error if there
-- is no entry with such a key.
withKey :: HistPL a b -> Key -> IO LexEntry
withKey hpl key = tryWithKey hpl key >>= maybe
    (fail $ "Failed to open entry with the " ++ show key ++ " key") return


-- | Lookup the form in the dictionary.
-- TODO: Apply word to the map in the result.
lookup :: HistPL a b -> T.Text -> IO [(LexEntry, a, M.Map D.Word b)]
lookup hpl x = do
    let entry = D.lookup x (formMap hpl)
    sequence
        [ (, a, forms) <$> withKey hpl (Key x uid)
        | (uid, (a, forms)) <- M.toList entry ]


--------------------------------------------------------
-- Conversion
--------------------------------------------------------


-- -- | Save the HistPL dictionary in the empty directory.
-- save
--     :: (Binary a, Binary b) => FilePath
--     -> [LexEntry]
--     -> IO ()
-- save path xs = do
--     createDirectoryIfMissing True path
--     isEmpty <- emptyDirectory path
--     when (not isEmpty) $ do
--         error $ "save: directory " ++ path ++ " is not empty"
--     let lexPath = path </> entryDir
--     createDirectory lexPath
--     formMap' <- D.fromListWith S.union . concat
--         <$> mapIO'Lazy (saveBin lexPath) (addKeys xs)
--     encodeFile (path </> formMapFile) formMap'
--   where
--     saveBin lexPath x = do
--         saveBinEntry lexPath x
--         return $ rules x
--     rules binEntry =
--         [ ( T.unpack x
--           , S.singleton (between x key) )
--         | x <- Util.allForms (lexEntry binEntry) ]
--       where
--         key = binKey binEntry
-- 
-- 
-- -- | Load dictionary from a disk in a lazy manner.  Raise an error
-- -- if the path doesn't correspond to a binary representation of the
-- -- dictionary. 
-- load :: FilePath -> IO [(Key, LexEntry)]
-- load path = tryLoad path >>=
--     maybe (fail "Failed to open the dictionary") return
-- 
-- 
-- -- | Load dictionary from a disk in a lazy manner.  Return 'Nothing'
-- -- if the path doesn't correspond to a binary representation of the
-- -- dictionary. 
-- tryLoad :: FilePath -> IO (Maybe [(Key, LexEntry)])
-- tryLoad path = runMaybeT $ do
--     hpl  <- MaybeT $ tryOpen path
--     -- lift $ mapM (withKey hpl) =<< getIndex hpl
--     keys <- getIndex hpl
--     lift $ sequence
--         [
--         | 


-- | Construct dictionary from a list of lexical entries and save it in
-- the given directory. 
-- To every entry an additional value of type @a@ will be assigned based
-- on the first given function.
-- Similarly, to every (entry, form) pair an additional value of type @b@
-- will be assigned based on the second given function.
save
    :: (Ord a, Binary a, Ord b, Binary b)
    => FilePath
    -> (LexEntry -> a)              -- ^ Determine value for the
                                    -- particular entry.
    -> (LexEntry -> D.Word -> b)    -- ^ Determine value for the particular
                                    -- (entry, word form) pair.
    -> [LexEntry]
    -> IO (HistPL a b)
save path f g xs = do
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
        let Key{..} = key
            infoE = f lexEntry
            infoW = g lexEntry
        return
            [ (keyForm, keyUid, infoE, y, infoW y)
            | y <- Util.allForms lexEntry ]


-- | Load all lexical entries in a lazy manner.
load :: HistPL a b -> IO [(Key, LexEntry)]
load hpl = do
    keys <- getIndex hpl
    forIO'Lazy keys $ \key -> do
        lexEntry <- withKey hpl key
        return (key, lexEntry)


-- | Load additional information about the entry.
loadInfo :: HistPL a b -> Key -> IO (a, M.Map D.Word b)
loadInfo = undefined
