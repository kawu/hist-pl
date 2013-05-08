{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import System.Console.CmdArgs
import Data.Binary (decodeFile)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import NLP.Polh.Binary (PolhM, runPolh)
import NLP.Polh.Analyse (Hist, anaText, showText)

data Analyse = Analyse 
    { polhPath      :: FilePath
    , anaPath       :: FilePath }
    deriving (Data, Typeable, Show)

analyse :: Analyse
analyse = Analyse
    { polhPath = def &= typ "Polh-Binary" &= argPos 0
    , anaPath  = def &= typ "Fused-DAWG" &= argPos 1 }

main :: IO ()
main = exec =<< cmdArgs analyse

exec :: Analyse -> IO ()

exec Analyse{..} = do
    dawg <- decodeFile anaPath
    xs <- L.lines <$> L.getContents 
    _ <- runPolh polhPath $ forM_ xs $ \line -> do
    	out <- onLine dawg line
	lift $ L.putStrLn out
    return ()

onLine :: Hist -> L.Text -> PolhM L.Text
onLine dawg line =
    showText <$> anaText dawg (L.toStrict line)
