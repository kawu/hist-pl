{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE TupleSections #-} 


{-|
    The module provides functions for working with the binary
    representation of the historical dictionary of Polish.

    It is intended to be imported qualified, to avoid name
    clashes with Prelude functions, e.g. 

    > import qualified NLP.HistPL.Lexicon as H
   
    Use `save` and `load` functions to save/load
    the entire dictionary in/from a given directory.
   
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

    Finally, if you need to follow an ID pointer kept in one entry
    as a reference to another one, use the `loadI` or `tryLoadI`
    functions.
-}


module NLP.HistPL.Lexicon
(
-- * Dictionary
  HistPL
, Code (..)
-- ** Key
, Key
, UID
-- ** Open
, tryOpen
, open

-- * Query
-- ** By Form
, lookup
, lookupMany
-- ** By Prefix
, nthSuffix
, withPrefix
-- ** By Key
, dictKeys
, tryLoadK
, loadK
-- ** By ID
, dictIDs
, tryLoadI
, loadI

-- * Conversion
, save
, load

-- * Modules
-- $modules
, module NLP.HistPL.Types
) where


import           Prelude hiding (lookup)
import           Control.Applicative ((<$>))
import           Control.Arrow (first, second)
import           Control.Monad (unless, guard, (<=<))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Control.Monad.Trans.State.Strict as S
import           Pipes
import qualified Pipes.Prelude as P
import           System.FilePath ((</>))
import           System.Directory
    ( createDirectoryIfMissing, createDirectory, doesDirectoryExist )
import           Data.List (foldl')
import           Data.Binary (Binary, put, get, encodeFile, decodeFile)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.DAWG.Dynamic as DM

import qualified NLP.HistPL.Binary as B
import           NLP.HistPL.Binary.Util
import qualified NLP.HistPL.DAWG as D
import           NLP.HistPL.Types
import qualified NLP.HistPL.Util as Util


{- $modules
    "NLP.HistPL.Types" module exports hierarchy of data types
    stored in the binary dictionary.
-}


--------------------------------------------------------
-- Subdirectories
--------------------------------------------------------


-- | Path to entries in the binary dictionary.
entryDir :: String
entryDir = "entries"


-- | Path to keys in the binary dictionary.
keyDir :: String
keyDir = "keys"


-- | Path to key map in the binary dictionary.
formFile :: String
formFile = "forms.bin"


--------------------------------------------------------
-- Key
--------------------------------------------------------


-- | A dictionary key which uniquely identifies the lexical entry.
type Key = D.Key UID


-- | A unique identifier among entries with the same `keyForm`.
type UID = Int


-- | The ''main form'' of the lexical entry.
proxy :: LexEntry -> T.Text
proxy entry = case Util.allForms entry of
    (x:_)   -> x
    []      -> error "proxy: entry with no forms"


-- | Convert the key to the path where binary representation of the entry
-- is stored.
showKey :: Key -> String
showKey D.Key{..} = (T.unpack . T.concat) [T.pack (show uid), "-", path]


-- | Parse the key.
parseKey :: String -> Key
parseKey x =
    let (uid'S, (_:form'S)) = break (=='-') x
    in  D.Key (T.pack form'S) (read uid'S)


--------------------------------------------------------
-- Keys storage
--------------------------------------------------------


-- | Save (key, lexID) pair in the keys component of the binary dictionary.
saveKey :: FilePath -> Key -> T.Text -> IO ()
saveKey path key i = T.writeFile (path </> keyDir </> showKey key) i


-- | Load lexID given the corresponding key.
loadKey :: FilePath -> Key -> IO T.Text
loadKey path key = T.readFile (path </> keyDir </> showKey key)


--------------------------------------------------------
-- Entry storage
--------------------------------------------------------


-- | Save entry in the binary dictionary.
saveEntry :: FilePath -> Key -> LexEntry -> IO ()
saveEntry path key x = do
    saveKey path key (lexID x)
    B.save (path </> entryDir) x


-- | Load entry from a disk by its key.
loadEntry :: FilePath -> Key -> IO LexEntry
loadEntry path = B.load (path </> entryDir) <=< loadKey path


-- | Load entry from a disk by its key.
tryLoadEntry :: FilePath -> Key -> IO (Maybe LexEntry)
tryLoadEntry path = maybeErr . loadEntry path


--------------------------------------------------------
-- Binary dictionary
--------------------------------------------------------


-- | A binary version of the old Polish dictionary.
data HistPL = HistPL {
    -- | A path to the binary dictionary.
      dictPath  :: FilePath
    -- | A dictionary with lexicon forms.
    , formMap   :: D.DAWG UID () Code
    }


-- | Code represents a word origin.  See the `save` function to
-- learn why do we provide this information.
data Code
    = Orig  -- ^ only from historical dictionary
    | Both  -- ^ from both historical and another dictionary
    | Copy  -- ^ only from another dictionary
    deriving (Show, Eq, Ord)


instance Binary Code where
    put Orig = put '1'
    put Copy = put '2'
    put Both = put '3'
    get = get >>= \x -> return $ case x of
        '1' -> Orig
        '2' -> Copy
        '3' -> Both
        c   -> error $ "get: invalid Code value '" ++ [c] ++ "'"


-- | Open the binary dictionary residing in the given directory.
-- Return Nothing if the directory doesn't exist or if it doesn't
-- constitute a dictionary.
tryOpen :: FilePath -> IO (Maybe HistPL)
tryOpen path = runMaybeT $ do
    formMap  <- maybeErrT $ decodeFile (path </> formFile)
    -- doesExist <- liftIO $ doesDirectoryExist (path </> entryDir)
    doesExist <- lift $ doesDirectoryExist (path </> entryDir)
    guard doesExist 
    return $ HistPL path formMap


-- | Open the binary dictionary residing in the given directory.
-- Raise an error if the directory doesn't exist or if it doesn't
-- constitute a dictionary.
open :: FilePath -> IO HistPL
open path = tryOpen path >>=
    maybe (fail "Failed to open the dictionary") return


-- | List of dictionary keys.
dictKeys :: HistPL -> Producer Key IO ()
dictKeys hpl = do
    let getPaths = getUsefulContents $ dictPath hpl </> keyDir
    xs <- map parseKey <$> lift getPaths
    each xs


-- | Load lexical entry given its key.  Raise error if there
-- is no entry with such a key.
loadK :: HistPL -> Key -> IO LexEntry
loadK hpl = loadEntry (dictPath hpl)


-- | Load lexical entry given its key.  Return `Nothing` if there
-- is no entry with such a key.
tryLoadK :: HistPL -> Key -> IO (Maybe LexEntry)
tryLoadK hpl = tryLoadEntry (dictPath hpl)


-- | List of dictionary IDs.
dictIDs :: HistPL -> Producer T.Text IO ()
dictIDs hpl = do
    let getPaths = getUsefulContents $ dictPath hpl </> entryDir
    xs <- map T.pack <$> lift getPaths
    each xs


-- | Load lexical entry given its ID.  Raise error if there
-- is no entry with such a key.
loadI :: HistPL -> T.Text -> IO LexEntry
loadI hpl i = B.load (dictPath hpl </> entryDir) i


-- | Load lexical entry given its ID.  Return `Nothing` if there
-- is no entry with such ID.
tryLoadI :: HistPL -> T.Text -> IO (Maybe LexEntry)
tryLoadI hpl i = B.tryLoad (dictPath hpl </> entryDir) i


-- | Lookup the form in the dictionary.
-- The resultant list constitutes a map from entries to `Code`s.
lookup :: HistPL -> T.Text -> IO [(LexEntry, Code)]
lookup hpl x = do
    let lexSet = D.lookup x (formMap hpl)
    sequence
        [ (   , code) <$> loadK hpl key
        | (key, code) <- getCode =<< M.assocs lexSet ]
  where
    getCode (key, val) =
        [ (key { D.path = base }, code)
        | (base, code) <- M.toList (D.forms val) ]
        

-- | Lookup a set of forms in the dictionary.
-- The resultant list constitutes a map from entries to `Code`s.
lookupMany :: HistPL -> [T.Text] -> IO [(LexEntry, Code)]
lookupMany hpl xs = do
    let keyMap = M.fromListWith min $
            getCode =<< M.assocs =<<
            (flip D.lookup (formMap hpl) <$> xs)
    sequence
        [ (   , code) <$> loadK hpl key
        | (key, code) <- M.toList keyMap ]
  where
    getCode (key, val) =
        [ (key { D.path = base }, code)
        | (base, code) <- M.toList (D.forms val) ]


-- | Get suffix of the `i`-th form starting with a given prefix.
nthSuffix :: HistPL -> T.Text -> Int -> Maybe T.Text
nthSuffix HistPL{..} x i = D.byIndex i (D.submap x formMap)


-- | Compute the number of entries with a given prefix.
withPrefix :: HistPL -> T.Text -> Int
withPrefix HistPL{..} x = D.size (D.submap x formMap)


--------------------------------------------------------
-- Conversion
--------------------------------------------------------


-- | Construct dictionary from a list of lexical entries and save it in
-- the given directory.  To each entry an additional set of forms can
-- be assigned.  The stream of entry pairs should be terminated by the
-- `Nothing` value.
save :: FilePath -> Consumer (Maybe (LexEntry, S.Set T.Text)) IO ()
save binPath = do

    -- Prepare directory for the dictionary.
    lift $ do
        createDirectoryIfMissing True binPath
        emptyDirectory binPath >>= \empty -> unless empty $ do
            error $ "save: directory " ++ binPath ++ " is not empty"
        createDirectory $ binPath </> entryDir
        createDirectory $ binPath </> keyDir

    formMap <- S.evalStateT loop s0
    lift $ encodeFile (binPath </> formFile) (D.weigh formMap)

  where

    loop = lift await >>= \x -> case x of
        Nothing -> D.freeze . fst <$> S.get
        Just (entry, forms) -> do
            key <- getKey entry
            saveBin key entry forms
            loop
    
    -- Empty, initial state.  The first state component represents the
    -- `formMap` initializer.  The second component is used to compute
    -- keys for individual entries.
    s0 = (D.empty, DM.empty)

    -- Compute key of the entry.
    getKey entry = do 
        km <- snd <$> S.get
        let main = proxy entry
            path = T.unpack main
            num  = maybe 0 id (DM.lookup path km) + (1 :: Int)
            key  = D.Key main num
        S.modify $ second $ DM.insert path num
        return key

    -- Save binary entry on a disk and update the map of forms.
    saveBin key entry otherForms = do
        lift $ lift $ saveEntry binPath key entry
        let D.Key{..} = key
            histForms = S.fromList (Util.allForms entry)
            onlyHist  = S.difference histForms otherForms
            onlyOther = S.difference otherForms histForms
            both      = S.intersection histForms otherForms
            list c s  = [(y, uid, (), path, c) | y <- S.toList s]
            xs        = list Orig onlyHist
                     ++ list Copy onlyOther
                     ++ list Both both
        -- TODO: Make it strict!
        S.modify $ first $ flip (foldl' (flip D.insert)) xs


-- | A producer of all dictionary entries.
load :: HistPL -> Producer (Key, LexEntry) IO ()
load hpl = dictKeys hpl >-> P.mapM (\x -> (x, ) <$> loadK hpl x)
