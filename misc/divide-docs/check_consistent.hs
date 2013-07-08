{-# LANGUAGE ScopedTypeVariables #-}

import           System.Environment (getArgs)
import           System.FilePath ((</>))
import qualified System.Directory as Dir
import           Control.Applicative ((<$>), (<|>))
import           Control.Monad (unless)
import qualified Data.Csv.Streaming as Csv
import qualified Data.Foldable as F
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S
import           Data.List (isPrefixOf)

-- | Word ID.
type ID = String

-- | Work title.
type Title = String

-- | A CSV entry.
type Entry = (ID, Title, FilePath)

-- | Make a set of identifiers from the CSV file.
mkIdSet :: F.Foldable t => t Entry -> S.Set ID
mkIdSet = flip F.foldl' S.empty $ \idSet (workID, _, _) ->
    S.insert workID idSet

-- | Check if there is a document corresponding to every
-- ID in the CSV file.
checkIdDoc
    :: FilePath     -- Root directory
    -> FilePath     -- CSV file
    -> IO ()
checkIdDoc rootPath csvPath = do
    putStrLn "# ID -> Path"
    csv <- Csv.decode True <$> BL.readFile csvPath
    F.for_ csv $ \(entry :: Entry) -> do
        let (workID, _, _) = entry
        b <- Dir.doesFileExist (rootPath </> workID)
        unless b $ putStrLn $ "Document '" ++ workID ++ "' doesn't exist"

-- | Check if there is an ID corresponding to every document
-- in the CSV file.
checkDocId
    :: FilePath     -- Root directory
    -> FilePath     -- CSV file
    -> IO ()
checkDocId rootPath csvPath = do
    putStrLn "# ID -> Path"
    idSet <- mkIdSet . Csv.decode True <$> BL.readFile csvPath
    files <- filesIn rootPath
    F.forM_ files $ \workID -> do
        unless (S.member workID idSet) $ do
            putStrLn $ "Entry '" ++ workID ++ "' doesn't exist"

----------------
-- Misc
----------------

-- | Retrieve all files from the given directory.
filesIn :: FilePath -> IO [FilePath]
filesIn path = filter (not . isPrefixOf ".")
           <$> Dir.getDirectoryContents path

----------------
-- Main
----------------

-- | Main function.
main = do
    [rootPath, csvPath] <- getArgs
    checkIdDoc rootPath csvPath
    checkDocId rootPath csvPath
