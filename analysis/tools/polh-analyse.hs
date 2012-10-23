{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import System.Console.CmdArgs
import Data.Binary (encodeFile, decodeFile)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import NLP.Polh.Analyse (Trie, buildTrie, anaSent)
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
    trie <- fmap D.fromDAWG $ decodeFile anaPath
    -- encodeFile outPath (D.fromTrie trie)
    L.interact (onLine trie)

onLine :: Trie -> L.Text -> L.Text
onLine trie line =
    let toks = anaSent trie (L.toStrict line)
    in  L.intercalate "\n" $ map (L.pack . show) toks
