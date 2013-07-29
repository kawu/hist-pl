{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text as T
import Options.Applicative
import Data.Binary (decodeFile)

import Data.PoliMorf (RelCode(..))
import Data.DAWG.Array (DAWGArray, size)

import Data.Polh.Collect (search, HistDict)

data Args = Args { dawgPath :: FilePath }

argsP :: Parser Args
argsP = Args <$> argument str ( metavar "POLI-HIST-DAWG" )

main :: IO ()
main = execParser opts >>= doTest
  where
    opts = info (helper <*> argsP)
      ( fullDesc
      & progDesc "Test approximate searchign in POLI-HIST-DAWG"
      & header "POLI-HIST-DAWG test" )

doTest :: Args -> IO ()
doTest Args{..} = do
    poliHist <- decodeFile dawgPath
    putStr "Dictionary size: "
    print $ size poliHist
    interact (unlines . map (test poliHist) . lines)

test :: HistDict -> String -> String
test poliHist line =
    let [x, k] = words line
    in  show $ search poliHist (read k) x
