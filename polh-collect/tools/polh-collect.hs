{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text as T
import Options.Applicative
import Data.Binary (decodeFile)

import Data.PoliMorf (RelCode(..))
import Data.DAWG.Array (DAWGArray, size)
import Text.Tokenize.Util.String (unHyphen)

import Data.Polh.Collect (HistDict, collect)

data Args = Args
    { polhBinPath   :: FilePath
    , dawgPath      :: FilePath
    , inputPath     :: FilePath
    , sourceID      :: String
    , doUnHyphen    :: Bool
    , doTranscript  :: Bool }

argsP :: Parser Args
argsP = Args
    <$> argument str ( metavar "POLH-BINARY" )
    <*> argument str ( metavar "POLI-HIST-DAWG" )
    <*> argument str ( metavar "INPUT" )
    <*> argument str ( metavar "SRC-ID" )
    <*> switch
        ( long "unHyphen"
        & short 'u'
        & help "Handle hyphens at line endings" )
    <*> switch
        ( long "transcript"
        & short 't'
        & help "Transcript the input text" )

main :: IO ()
main = execParser opts >>= doCollect
  where
    opts = info (helper <*> argsP)
      ( fullDesc
      & progDesc ( "Collect new word forms from INPUT using POLI-HIST-DAWG\n"
              ++ "  to find approximate matchings and insert them to a\n"
              ++ "  POLH-BINARY lexicon." )
      & header "hello - a test for optparse-applicative" )

decodeDict :: FilePath -> IO HistDict
decodeDict = decodeFile

doCollect :: Args -> IO ()
doCollect Args{..} = do
    poliHist <- decodeDict dawgPath
    putStr "Dictionary size: "
    print $ size poliHist

    let h = if doUnHyphen then unHyphen else id
    xs <- h <$> readFile inputPath
    collect polhBinPath poliHist (T.pack sourceID) doTranscript xs
