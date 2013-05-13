{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative ((<$>))
import           Control.Monad (void)
import           System.Console.CmdArgs
import qualified Data.Set as S
import qualified Data.Map as M

import qualified Data.PoliMorf as P
import           NLP.HistPL.LMF (readLMF)
import qualified NLP.HistPL.Dict as D
import qualified NLP.HistPL as H
import qualified NLP.HistPL.Fusion as F

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
  deriving (Data, Typeable, Show)



createMode :: HistPL
createMode = Create
    { lmfPath  = def &= typ "HistPL-LMF" &= argPos 0
    , poliPath = def &= typ "PoliMorf" &= argPos 1
    , outPath  = def &= typ "HistPL-Binary" &= argPos 2 }


argModes :: Mode (CmdArgs HistPL)
argModes = cmdArgsMode $ modes
    [createMode]
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
    hist <- readLMF lmfPath
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
