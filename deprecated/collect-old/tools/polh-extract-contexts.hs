{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.IO as T
import Control.Monad (forM_)
import Options.Applicative

import Data.Polh.Types
import Text.Polh.Parse (parsePolh)

data Args = Args { lmfPath :: FilePath }

argsP :: Parser Args
argsP = Args <$> argument str ( metavar "LMF" )

main :: IO ()
main = execParser opts >>= doExtract
  where
    opts = info (helper <*> argsP)
      ( fullDesc
      & progDesc "Extract contexts from historical LMF lexicon"
      & header "polh-extract-contexts" )

doExtract :: Args -> IO ()
doExtract Args{..} = do
    polh <- parsePolh <$> L.readFile lmfPath
    forM_ polh $ \entry -> mapM_ T.putStrLn
        [ writtenForm r
        | context <- cxts =<< senses entry
        , r <- repr context, language r == "polh" ]
