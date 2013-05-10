{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative ((<$>))
import           System.Console.CmdArgs
import           Data.Binary (encodeFile)

import qualified Data.PoliMorf as P
import           NLP.HistPL.Dict (revDict)
import qualified NLP.HistPL as H
import qualified NLP.HistPL.Fusion as F

data HistPL_Fuse = HistPL_Fuse
    { histPath      :: FilePath
    , poliPath      :: FilePath
    , outPath       :: FilePath }
  deriving (Data, Typeable, Show)

histFuse :: HistPL_Fuse
histFuse = HistPL_Fuse
    { histPath = def &= typ "HistPL-Binary" &= argPos 0
    , poliPath = def &= typ "PoliMorf" &= argPos 1
    , outPath  = def &= typ "Output-Analysis-DAWG" &= argPos 2 }

main :: IO ()
main = exec =<< cmdArgs histFuse

exec :: HistPL_Fuse -> IO ()
exec HistPL_Fuse{..} = do
    poli <- F.mkPoli . filter P.atomic <$> P.readPoliMorf poliPath
    hpl <- H.open histPath
    hist <- F.mkHist <$> H.load hpl
    let dict = F.fuse corr hist poli
    encodeFile outPath (revDict dict)
  where
    corr = F.buildCorresp F.byForms F.posFilter F.sumChoice
