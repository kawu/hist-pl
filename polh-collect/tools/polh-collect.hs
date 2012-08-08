{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import qualified Data.Text as T
import Data.Binary (decodeFile)

import Data.PoliMorf (RelCode(..))
import Data.DAWG.Array (DAWGArray, size)
import Text.Tokenize.Util.String (unHyphen)

import Data.Polh.Collect (collect)

type ID = T.Text
type HistDict = DAWGArray (Maybe (Maybe (ID, RelCode)))

decodeDict :: FilePath -> IO HistDict
decodeDict = decodeFile

main = do
    [polhBinPath, dawgPath, impactPath] <- getArgs

    poliHist <- decodeDict dawgPath
    putStr "Dictionary size: "
    print $ size poliHist

    xs <- unHyphen <$> readFile impactPath
    collect polhBinPath poliHist "impact" xs
