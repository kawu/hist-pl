{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import System.Console.CmdArgs
import Data.Binary (encodeFile, decodeFile)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import NLP.Polh.Analyse (Trie, buildTrie, anaText, showText)
import NLP.Polh.Binary (PolhM, runPolh)
import qualified NLP.Adict.DAWG as D

data Args
  = LexMode
    { polhPath      :: FilePath
    , poliPath      :: FilePath
    , outPath       :: FilePath }
  | AnaMode
    { polhPath      :: FilePath
    , anaPath       :: FilePath }
  deriving (Data, Typeable, Show)

lexMode :: Args
lexMode = LexMode
    { polhPath = def &= typ "Polh-Binary" &= argPos 0
    , poliPath = def &= typ "PoliMorf" &= argPos 1
    , outPath  = def &= typ "Output-Analysis-DAWG" &= argPos 2 }

anaMode :: Args
anaMode = AnaMode
    { polhPath = def &= typ "Polh-Binary" &= argPos 0
    , anaPath  = def &= typ "Analysis-DAWG" &= argPos 1 }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [lexMode, anaMode]

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Args -> IO ()

exec LexMode{..} = do
    trie <- buildTrie polhPath poliPath
    encodeFile outPath (D.fromTrie trie)

exec AnaMode{..} = do
    trie <- D.fromDAWG <$> decodeFile anaPath
    xs <- L.lines <$> L.getContents 
    _ <- runPolh polhPath $ forM_ xs $ \line -> do
    	out <- onLine trie line
	lift $ L.putStrLn out
    return ()

onLine :: Trie -> L.Text -> PolhM L.Text
onLine trie line =
    showText <$> anaText trie (L.toStrict line)
