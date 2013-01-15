{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>))
import System.Console.CmdArgs
import Data.Binary (encodeFile)

import qualified NLP.Polh.Binary as H
import qualified Data.PoliMorf as P
import qualified NLP.Polh.Fusion as F

data Polh_Fuse = Polh_Fuse
    { polhPath      :: FilePath
    , poliPath      :: FilePath
    , outPath       :: FilePath }
  deriving (Data, Typeable, Show)

polhFuse :: Polh_Fuse
polhFuse = Polh_Fuse
    { polhPath = def &= typ "Polh-Binary" &= argPos 0
    , poliPath = def &= typ "PoliMorf" &= argPos 1
    , outPath  = def &= typ "Output-Analysis-DAWG" &= argPos 2 }

main :: IO ()
main = exec =<< cmdArgs polhFuse

exec :: Polh_Fuse -> IO ()
exec Polh_Fuse{..} = do
    poli <- F.fromPoli . filter P.atomic <$> P.readPoliMorf poliPath
    hist <- H.loadPolh polhPath >>= \x -> case x of
    	Nothing -> error "polh-fuse: not a binary historical dictionary"
	Just xs -> return $ F.mkHist xs
    let dawg = F.fuse corr hist poli
    encodeFile outPath dawg
  where
    corr = F.buildCorresp F.byForms F.posFilter F.sumChoice
