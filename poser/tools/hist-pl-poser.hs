{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           System.Console.CmdArgs
import           NLP.HistPL.Poser


---------------------------------------
-- Command line options
---------------------------------------


-- | Program arguments.
data PArgs = PArgs
    { src   :: FilePath
    , dst   :: FilePath }
    deriving (Data, Typeable, Show)


pArgs :: PArgs
pArgs = PArgs
    { src = def &= typ "SRC-LMF" &= argPos 0
    , dst = def &= typ "DST-LMF" &= argPos 1 }
    &= verbosity
    &= summary "Assign POS tags"
    &= program "hist-pl-poser"


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgs pArgs


exec :: PArgs -> IO ()
exec PArgs{..} = vote src dst
