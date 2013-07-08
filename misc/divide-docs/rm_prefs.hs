import           System.Environment (getArgs)
import           Control.Applicative ((<$>))
import           Control.Monad (unless, forM_)
import qualified System.Directory as Dir
import           System.FilePath
import           Data.List (isPrefixOf)

-- | Retrive all files from the given directory.
filesIn :: FilePath -> IO [FilePath]
filesIn path = filter (not . isPrefixOf ".")
           <$> Dir.getDirectoryContents path

-- | Check, if the given directory is empty.
isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty = fmap null . filesIn

-- | Main function.
main = do
    [srcPath, dstPath] <- getArgs

    Dir.doesDirectoryExist srcPath >>= \b -> unless b
        (error "Source directory doesn't exist")
    Dir.doesDirectoryExist dstPath >>= \b -> unless b
        (error "Destination directory doesn't exist")
    isDirectoryEmpty dstPath >>= \b -> unless b
        (error "Destination directory is not empty")

    files <- filesIn srcPath
    forM_ files $ \path -> do
        let path' = drop 11 path
        Dir.copyFile (srcPath </> path) (dstPath </> path')
