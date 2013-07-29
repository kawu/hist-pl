-- | A module responsible for reading texts stored in different formats.


module NLP.HistPL.Collect.Read
(
-- * Collection
  readColl

-- * Reading words
, readWords
) where


import           Prelude hiding (words)
import           System.Directory (getDirectoryContents)
import           System.FilePath
import           Control.Applicative ((<$>))
import           Data.Function (on)
import qualified Data.Char as C
import           Data.Maybe (isJust)
import qualified Data.List as L
import           Data.List.Split (splitWhen)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT


--------------------------------------------
-- Collection of documents
--------------------------------------------


-- | Read collection of documents from the given directory.
readColl :: FilePath -> IO [FilePath]
readColl path = do
    xs <- filter (\x -> not (x `elem` [".", ".."]))
      <$> getDirectoryContents path
    return $ map (path </>) xs


--------------------------------------------
-- Reading words
--------------------------------------------


-- -- | Reading configuration.
-- data ReadConf = ReadConf


-- | Read words from the given file.
readWords :: FilePath -> IO [T.Text]
readWords path
    = concatMap (words . LT.toStrict)
    . LT.lines <$> LT.readFile path



-- -- | Read words from the given file together with corresponding occurence
-- -- contexts.
-- readWordsWithCxts


--------------------------------------------
-- Divide into words
--------------------------------------------


-- Rules of custom tokenization:
-- - Filter out words, which contain punctuation markers *inside*,
--   e.g. "zapłacz[e]my".


-- | Divide the text into words w.r.t. the configuration.
words :: T.Text -> [T.Text]
words
    = concatMap tokGroup
    . splitWhen isSpace 
    . tokenize


-- The result will contain only text, or 'Nothing', if the group
-- doesn't satisfy the above-specified tokenization rules.
tokGroup :: [T.Text] -> [T.Text]
tokGroup x
    | otherInside x     = []
    | otherwise         = filter isText x


-- | Other element between text parts?
otherInside :: [T.Text] -> Bool
otherInside = isJust . L.find isOther
            . L.dropWhile isOther
            . L.dropWhileEnd isOther


--------------------------------------------
-- Custom tokenization
--------------------------------------------


data TextType
    = Text
    | Space
    | Other
    deriving (Show, Eq, Ord)


-- | Letter type.
letterType :: Char -> TextType
letterType x
    | C.isLetter x  = Text
    | C.isSpace x   = Space
    | otherwise     = Other


-- | Text type.
textType :: T.Text -> TextType
textType x
    | T.any C.isLetter x   = Text
    | T.any C.isSpace x    = Space
    | otherwise            = Other


isSpace :: T.Text -> Bool
isSpace = (==Space) . textType


isText :: T.Text -> Bool
isText = (==Text) . textType


isOther :: T.Text -> Bool
isOther = (==Other) . textType


-- | Perform simple tokenization -- spaces and punctuation
-- characters are treated as token ending markers.
tokenize :: T.Text -> [T.Text]
tokenize = T.groupBy ((==) `on` letterType)


--------------------------------------------
-- Misc
--------------------------------------------
