{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative ((<$>))
import           Control.Monad (forever, void, forM_, (<=<))
import           Pipes
import           System.Console.CmdArgs
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson as Aeson

import qualified Data.PoliMorf as P
import qualified NLP.HistPL.LMF as LMF
import qualified NLP.HistPL.DAWG as D
import qualified NLP.HistPL.Lexicon as H
import qualified NLP.HistPL.Fusion as F
import qualified NLP.HistPL.Analyse as A

import qualified NLP.HistPL.Transliter.Impact as I

import           Paths_hist_pl (version)
import           Data.Version (showVersion)


---------------------------------------
-- Command line options
---------------------------------------


-- | A description of the hist-pl tool
histDesc :: String
histDesc = "HistPL " ++ showVersion version


data HistPL
  = Create
    { lmfPath       :: FilePath
    , poliPath      :: FilePath
    , outPath       :: FilePath }
  | Print
    { binPath       :: FilePath }
  | Analyse
    { binPath       :: FilePath
    , transFlag     :: Bool
    , rmHypFlag     :: Bool
    , compact       :: Bool
    , printCont     :: Int
    , printDefs     :: Bool }
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
    { binPath = def &= typ "HistPL-Binary" &= argPos 0
    , transFlag = False &= (help . unwords)
        [ "Perform pre-transliteration using the set of rules prepared for"
        , "IMPACT documents." ]
    , rmHypFlag = False &= (help . unwords)
        [ "Remove all instances of the \"-\\n\" string."
        , "Useful with IMPACT documents." ]
    , compact = False &= help "Compact JSON output"
    , printCont = 1 &= (help . unwords)
        [ "Printing contemporary interpretations:"
        , "0 -- never, 1 -- when no hist (default), 2 -- always." ]
    , printDefs = False &= help "Print definitions." }


argModes :: Mode (CmdArgs HistPL)
argModes = cmdArgsMode $ modes
    [createMode, printMode, anaMode]
    &= summary histDesc
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
    runEffect $ fromListI (addForms poli hist) >-> H.save outPath
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
    runEffect $ for (H.load hpl) (lift . L.putStrLn . LMF.showLexEntry . snd)


exec Analyse{..} = do
    hpl <- H.open binPath
    xs  <- L.lines . rmHyp <$> L.getContents 
    forM_ xs $ BC.putStrLn <=< onLine hpl
  where
    rmHyp | rmHypFlag = A.rmHyphen
          | otherwise = id
    onLine hpl
        = fmap (encode . A.jsonAna jsonConf)
        . A.mapL (A.anaWord hpl . trans)
        . A.tokenize . L.toStrict
    trans   | transFlag = T.pack . I.transliter I.impactRules . T.unpack
            | otherwise = id
    encode  | compact   = Aeson.encode
            | otherwise = Aeson.encodePretty
    jsonConf = A.defaultJConf
        { A.showCont = case printCont of
            0   -> A.NoShowCont
            2   -> A.ForceShowCont
            _   -> A.ShowCont
        , A.showDefs = printDefs }


---------------------------------------
-- Misc
---------------------------------------


fromListI :: (Monad m) => [a] -> Producer (Maybe a) m r
fromListI xs = do
    mapM_ (yield . Just) xs
    forever $ yield Nothing
