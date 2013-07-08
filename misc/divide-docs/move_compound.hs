import           System.Environment (getArgs)
import           Control.Applicative ((<$>), (<|>))
import           Control.Monad (when, unless, forM)
import           Control.Error
import           Control.Monad.Trans.Class (lift)
import           Data.Maybe (catMaybes)
import qualified Data.Char as C
import qualified Data.Csv.Streaming as Csv
import qualified Data.Foldable as F
import qualified System.Directory as Dir
import qualified System.FilePath.Windows as WP
import qualified System.FilePath.Posix as PP
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Debug.Trace (trace)

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
    -> FilePath                 -- A compound path
    -> FilePath                 -- A destination directory
    -> (FilePath, S.Set (ID, Title))    -- Index entry; the path is kept
                                        -- using the Windows convention.
    -> IO ()
process root compPath dest (wpath, workSet) = printLeft $ runEitherT $ do
    let path = PP.joinPath
             $ map WP.dropTrailingPathSeparator
             $ WP.splitPath wpath
    lift (Dir.doesFileExist $ root PP.</> path) >>= tryAssert
        ("[0] Path " ++ quoted path ++ " doesn't exist")
    when (S.size workSet > 1 && compPath == wpath) $ lift $
        processCompound root dest path workSet

-- | Process compound document which consists of one entry.
processCompound
    :: FilePath                 -- A root dir
    -> FilePath                 -- A destination dir
    -> FilePath                 -- Work path
    -> S.Set (ID, Title)        -- A set of corresponding works
    -> IO ()
processCompound root dest path workSet = do
    cuts <- filterCuts =<< findCuts workSet
        <$> T.readFile (root PP.</> path)
    moveWorks cuts =<< T.readFile (root PP.</> path)
  where
    moveWorks cuts text = sequence_
        [ moveWork
            (dest PP.</> (showJust i ++ "-" ++ workID))
            (range i j text)
        | (i, j, (workID, title))  <- mkRanges
            (T.length text) (M.toList cuts') ]
      where
        cuts' = M.fromList . map swap . M.toList $ cuts
        swap (x, y) = (y, x)
    range i j = T.take (j-i) . T.drop i

mkRanges :: Int -> [(Int, a)] -> [(Int, Int, a)]
mkRanges n xs =
    [ (i, j, x)
    | ((i, x), (j, _)) <- pairs xs' ]
  where
    pairs xs = zip xs (tail xs)
    xs' = setLast n . setFirst 0 $ xs
    setFirst i ((_, x):xs) = ((i, x):xs)
    setFirst _ []          = []
    setLast i ((j, x):[])  = [(j, x), (i, x)]
    setLast i (x:xs)       = x : setLast i xs
    setLast _ []           = []
    
-- | Save work in the given directory.
moveWork :: FilePath -> T.Text -> IO ()
moveWork = T.writeFile

-- | Show right-justified number.
showJust :: Int -> String
showJust = T.unpack . T.justifyRight 10 '0' . T.pack . show

-- | Filter out works, for which no cutting point have been found.
filterCuts :: M.Map (ID, Title) (Maybe Int) -> IO (M.Map (ID, Title) Int)
filterCuts cuts = do
    fmap (M.fromList . catMaybes) .
        forM (M.toList cuts) $ \(work, mcut) -> case mcut of
            Nothing  -> do
                putStrLn $ "Not found " ++ show work
                return Nothing
            Just cut -> do
                putStrLn $ "Found " ++ show work
                return $ Just (work, cut)

-- | Find cutting points for the set of works in the given text.
findCuts :: S.Set (ID, Title) -> T.Text -> M.Map (ID, Title) (Maybe Int)
findCuts s text = M.fromList
    [ ( (workID, title)
      , T.strip (T.pack title) `findOcc` textPair )
    | (workID, title) <- S.toList s ]
  where
    textLower = T.toLower text
    textPair = (text, textLower)

-- | Find occurence position of text in the second text.
findOcc :: T.Text -> (T.Text, T.Text) -> Maybe Int
findOcc x (y, yl) =
        T.toUpper x `findAsLine` y
    <|> T.toUpper x `findIn` y
    <|>           x `findAsLine` y
    <|>           x `findIn` y
--     <|> (if length (T.words x) > 2
--             then (  T.toLower x `findAsLine` yl
--                 <|> T.toLower x `findIn` yl )
--             else Nothin )

----------------
-- Misc
----------------

-- | Find occurence position of text in the second text.
findIn :: T.Text -> T.Text -> Maybe Int
findIn x = breakPoint . T.breakOn x

-- | Find occurence position of text in the second text.
-- Return `Nothing`, if the matched text doesn't span over
-- the entire line.
findAsLine :: T.Text -> T.Text -> Maybe Int
findAsLine x y =
    let isLine (l, r) = firstVoid (T.reverse l)
                     && firstVoid (T.drop (T.length x) r)
    in  breakPoint =<< L.find isLine (T.breakOnAll x y)
        
-- | Is the first line of the given text a void line?
firstVoid :: T.Text -> Bool
firstVoid t = case maybeHead (T.lines t) of
    Nothing -> True
    Just x  -> T.length (T.strip x) == 0

-- | Compute position of the break.
breakPoint :: (T.Text, T.Text) -> Maybe Int
breakPoint (x, y) = if T.null y
    then Nothing
    else Just (T.length x)

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

maybeHead :: [a] -> Maybe a
maybeHead (x:xs) = Just x
maybyHead []     = Nothing

----------------
-- Main
----------------

-- | Main function.
main = do
    [rootPath, csvPath, compPath, destPath] <- getArgs

    Dir.doesDirectoryExist destPath >>= \b -> unless b
        (error "Destination directory doesn't exist")
    isDirectoryEmpty destPath >>= \b -> unless b
        (error "Destination directory is not empty")
      
    idMap <- mkIdMap . Csv.decode True <$> BL.readFile csvPath
    mapM_ (process rootPath compPath destPath) (M.toList idMap)
