module NLP.Polh.Binary
( savePolh
, loadPolh
, loadIndex
, loadLexEntry
, updateLexEntry
, updateLexEntry_
, lexKey
) where

import Control.Monad (forM_)
import Control.Monad.Lazy (forM)
import Control.Applicative ((<$>), (<*>))
import System.FilePath ((</>))
import System.Directory ( getDirectoryContents, doesFileExist
                        , doesDirectoryExist )
import Data.Binary (encodeFile, decodeFile, Binary)
import qualified Data.Text as T

import NLP.Polh.Types

type Key = String

-- doesExist :: FilePath -> IO Bool
-- doesExist path = do
--     x <- doesFileExist path
--     y <- doesDirectoryExist path
--     return (x || y)

loadIndex :: FilePath -> IO [Key]
loadIndex path = do
    xs <- getDirectoryContents path
    return [x | x <- xs, x /= ".", x /= ".."]

lexKey :: LexEntry -> Key
lexKey = T.unpack . lexId

saveOne :: Binary a => FilePath -> Key -> a -> IO ()
saveOne path elemId x = do
    let filePath = path </> elemId
--     doesExist filePath >>= \b -> case b of
--         True  -> fail (filePath ++ " already exists")
--         False -> return ()
    encodeFile filePath x

-- | Save a list of serializable values to a disk. Identification function
-- (a -> b) will be used to acquire file names for subsequent elements.
-- Function assumes that input path exists and that it is empty.
save :: Binary a => FilePath -> (a -> String) -> [a] -> IO ()
save path f xs = forM_ xs $ \x -> saveOne path (f x) x

loadOne :: Binary a => FilePath -> Key -> IO a
loadOne path elemId = decodeFile (path </> elemId)

-- | Load the dictionary in a lazy (unsafe!) manner.
load :: Binary a => FilePath -> IO [a]
load path = do
    xs <- loadIndex path
    forM xs (loadOne path)

saveLexEntry :: FilePath -> LexEntry -> IO ()
saveLexEntry path lexEntry = saveOne path (lexKey lexEntry) lexEntry

loadLexEntry :: FilePath -> Key -> IO LexEntry
loadLexEntry = loadOne

updateLexEntry :: FilePath -> Key -> (LexEntry -> LexEntry) -> IO LexEntry
updateLexEntry path lexKey f = do
    lexEntry <- loadLexEntry path lexKey
    let lexEntry' = f lexEntry
    saveLexEntry path lexEntry'
    return lexEntry'

updateLexEntry_ :: FilePath -> Key -> (LexEntry -> LexEntry) -> IO ()
updateLexEntry_ path lexKey f = do
    lexEntry <- loadLexEntry path lexKey
    saveLexEntry path (f lexEntry)

-- | Save the polh dictionary.
savePolh :: FilePath -> Polh -> IO ()
savePolh path = save path lexKey

-- | Load the dictionary in a lazy (unsafe!) manner.
loadPolh :: FilePath -> IO Polh
loadPolh = load
