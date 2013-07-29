{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative ((<$>))
import           Control.Monad (void, forM_, (<=<))
import           System.Console.CmdArgs
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

-- import qualified Data.PoliMorf as P
-- import qualified NLP.HistPL.LMF as LMF
-- import qualified NLP.HistPL.DAWG as D
-- import qualified NLP.HistPL.Lexicon as H
-- import qualified NLP.HistPL.Fusion as F
-- import qualified NLP.HistPL.Analyse as A
-- import qualified NLP.HistPL.Transliter.Impact as I

import qualified NLP.HistPL.Collect.Case as H
import qualified NLP.HistPL.Collect.Read as H

import           Paths_hist_pl_collect (version)
import           Data.Version (showVersion)


---------------------------------------
-- Command line options
---------------------------------------


-- | A description of the hist-pl tool
histDesc :: String
histDesc = "HistPL-Collect " ++ showVersion version


data HistPL
  = Collect
    { dataDir       :: FilePath }
  | LowerCase
    { inputPath     :: FilePath
    , upperFreq     :: Double }
  deriving (Data, Typeable, Show)


collectMode :: HistPL
collectMode = Collect
    { dataDir  = def &= typ "DATA-DIR" &= argPos 0 }


lowerCaseMode :: HistPL
lowerCaseMode = LowerCase
    { inputPath = def &= typ "WORDS-FILE" &= argPos 0
    , upperFreq = 0.9 &= help "Minimal frequency of upper-cased words" }


argModes :: Mode (CmdArgs HistPL)
argModes = cmdArgsMode $ modes
    [collectMode, lowerCaseMode]
    &= summary histDesc
    &= program "hist-pl-collect"


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgsRun argModes


exec :: HistPL -> IO ()
exec Collect{..} = do
    paths <- H.readColl dataDir
    forM_ paths $ \path -> do
        xs <- H.readWords path
        mapM_ T.putStrLn xs


exec LowerCase{..} = do
    let readWords path = map L.toStrict . L.lines <$> L.readFile path
    detCase <- H.detCase upperFreq <$> readWords inputPath
    readWords inputPath >>= \xs -> forM_ xs $ \x -> do
        T.putStrLn $ detCase x
