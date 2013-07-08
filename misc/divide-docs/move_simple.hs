import           System.Environment (getArgs)
import           Control.Applicative ((<$>))
import           Control.Monad (unless, forM)
import           Control.Error
import           Control.Monad.Trans.Class (lift)
import           Data.Maybe (catMaybes)
import qualified Data.Char as C
import qualified Data.Csv.Streaming as Csv
import qualified Data.Foldable as F
import qualified System.Directory as Dir
import qualified System.FilePath.Windows as WP
import qualified System.FilePath.Posix as PP
import qualified Data.ByteString.Lazy as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Word ID.
type ID = String

-- | Work title.
type Title = String

-- | A map from paths to works.
type IdMap = M.Map FilePath (S.Set (ID, Title))

-- | Make `IdMap` from a CSV file.
mkIdMap :: F.Foldable t => t (ID, Title, FilePath) -> IdMap
mkIdMap = flip F.foldl' M.empty $ \idMap (workID, title, path) ->
    M.insertWith S.union path (S.singleton (workID, title)) idMap

-- | Process index entry.
process
    :: FilePath                 -- A root directory
    -> FilePath                 -- A destination directory
    -> (FilePath, S.Set (ID, Title))    -- Index entry; the path is kept
                                        -- using the Windows convention.
    -> IO ()
process root dest (wpath, workSet) = printLeft $ runEitherT $ do
    let path = PP.joinPath
             $ map WP.dropTrailingPathSeparator
             $ WP.splitPath wpath
    lift (Dir.doesFileExist $ root PP.</> path) >>= tryAssert
        ("[0] Path " ++ quoted path ++ " doesn't exist")
    lift $ if S.size workSet == 1
        then processSimple root dest path (S.findMin workSet)
        else putStrLn $ "[2] Compound path " ++ quoted path ++ " found"

-- | Process simple document which consists of one entry.
processSimple
    :: FilePath                 -- A root dir
    -> FilePath                 -- A destination dir
    -> FilePath                 -- Work path
    -> (ID, Title)
    -> IO ()
processSimple root dest path (workID, _) = do
    Dir.copyFile (root PP.</> path) (dest PP.</> workID)
    putStrLn $ "[1] Path " ++ quoted path ++ " processed"

-- Return quoted string.
quoted :: String -> String
quoted x = "\"" ++ x ++ "\""

-- | Fail on left value.
printLeft :: IO (Either String a) -> IO ()
printLeft m = m >>= \x -> case x of                
    Left x  -> putStrLn x
    Right _ -> return ()

-- | Check, if the given directory is empty.
isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty path = do
    xs <- Dir.getDirectoryContents path
    return $ length xs <= 2

-- | Main function.
main = do
    [rootPath, csvPath, destPath] <- getArgs

    Dir.doesDirectoryExist destPath >>= \b -> unless b
        (error "Destination directory doesn't exist")
    isDirectoryEmpty destPath >>= \b -> unless b
        (error "Destination directory is not empty")
      
    idMap <- mkIdMap . Csv.decode True <$> L.readFile csvPath
    mapM_ (process rootPath destPath) (M.toList idMap)
