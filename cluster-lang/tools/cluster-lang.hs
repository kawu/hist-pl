{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
import           System.Console.CmdArgs

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L

import qualified Data.PoliMorf as P
import qualified NLP.Adict as A

import qualified NLP.ClusterLang as CL


---------------------------------------
-- Command line options
---------------------------------------


-- | Edit distance type to use.
data EditDist
    = Levenshtein   -- ^ Levenshtein distance.
    | Specialized   -- ^ Distance specialized for morphologically rich languages. 
                    -- Beware, though, it's not a metric!
    deriving (Eq,Ord,Bounded,Enum,Show,Read,Data,Typeable)


-- | Translate `EditDist` flag to a cost function.
costFrom :: EditDist -> A.Cost Char
costFrom = const A.costDefault


-- | Program arguments.
data CLArgs = CLArgs
    { minPts    :: Int
    , eps       :: Double
    , editDist  :: EditDist
    , output    :: FilePath }
    deriving (Data, Typeable, Show)


clArgs :: CLArgs
clArgs = CLArgs
    { minPts    = 3 &= help "Minimum number of points required to form a cluster"
    , eps       = 1.0 &= help "A parameter used to identify neighbors of an element"
    , editDist  = Levenshtein &= help "Type of edit distance"
    , output    = def &= argPos 0 &= typ "OUTPUT-FILE" }
    &= verbosity
    &= summary "Cluster language"
    &= program "cluster-lang"


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgs clArgs


exec :: CLArgs -> IO ()
exec CLArgs{..} = do
    xs <- map (T.unpack . P.form)
        . P.parsePoliMorf <$> L.getContents
    let cfg = CL.Conf
            { CL.minPts = minPts
            , CL.eps = eps
            , CL.cost = costFrom editDist }
    forM_ (CL.cluster cfg xs) $ \(x, y) -> do
        putStr x >> putStr " => " >> putStrLn y
