{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative ((<$>))
import           Control.Monad (void, forM_, (<=<))
import           System.Console.CmdArgs
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Data.PoliMorf as P
import qualified NLP.HistPL.LMF as LMF
import qualified NLP.HistPL.Dict as D
import qualified NLP.HistPL.Lexicon as H
import qualified NLP.HistPL.Fusion as F
import qualified NLP.HistPL.Analyse as A

import           Paths_hist_pl (version)
import           Data.Version (showVersion)


---------------------------------------
-- Command line options
---------------------------------------


-- | A description of the Concraft-pl tool
concraftDesc :: String
concraftDesc = "HistPL " ++ showVersion version


data HistPL
  = Create
    { lmfPath       :: FilePath
    , poliPath      :: FilePath
    , outPath       :: FilePath }
  | Print
    { binPath       :: FilePath }
  | Analyse
    { binPath       :: FilePath }
  deriving (Data, Typeable, Show)


createMode :: HistPL
createMode = Create
    { lmfPath  = def &= typ "HistPL-LMF" &= argPos 0
    , poliPath = def &= typ "PoliMorf" &= argPos 1
    , outPath  = def &= typ "HistPL-Binary" &= argPos 2 }


printMode :: HistPL
printMode = Print
    { binPath = def &= typ "HistPL-Binary" &= argPos 0 }


anaMode :: HistPL
anaMode = Analyse
    { binPath = def &= typ "HistPL-Binary" &= argPos 0 }


argModes :: Mode (CmdArgs HistPL)
argModes = cmdArgsMode $ modes
    [createMode, printMode, anaMode]
    &= summary concraftDesc
    &= program "hist-pl"


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgsRun argModes


exec :: HistPL -> IO ()
exec Create{..} = do
    -- putStrLn "Reading PoliMorf..."
    poli <- F.mkPoli . filter P.atomic <$> P.readPoliMorf poliPath
    -- putStrLn "Reading historical dictionary of Polish..."
    hist <- LMF.readLMF lmfPath
    -- putStrLn "Creating the binary version of the dictionary..."
    void $ H.save outPath (addForms poli hist)
  where
    addForms poli hist =
        [ ( lexEntry
          , formSet (corr poli lexEntry) )
        | lexEntry <- hist ]
    corr = F.buildCorresp F.byForms F.posFilter F.sumChoice
    formSet lexSet = S.fromList $ concat
        [ M.keys (D.forms val)
        | val <- M.elems lexSet ]


exec Print{..} = do
    hpl <- H.open binPath
    H.load hpl >>= L.putStr . LMF.showLMF . map snd


exec Analyse{..} = do
    hpl <- H.open binPath
    xs  <- L.lines <$> L.getContents 
    forM_ xs $ L.putStrLn <=< onLine hpl
  where
    onLine hpl x = A.showAna <$> A.anaText hpl (L.toStrict x)
