{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative ((<$>))
-- import           Control.Monad (forM_)
import           System.Console.CmdArgs

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L

import qualified Data.PoliMorf as P
import qualified NLP.Adict as A

import qualified NLP.ClusterLang as CL
import           NLP.ClusterLang.SpecialCost


---------------------------------------
-- Command line options
---------------------------------------


-- | Edit distance type to use.
-- Beware, that some types do not represent a metric!
data EditDist
    = Levenshtein   -- ^ Levenshtein distance.
    | Specialized   -- ^ Distance specialized for
                    -- morphologically rich languages. 
    | PosMod        -- ^ Standard cost function with
                    -- additional position modifier.
    deriving (Eq, Ord, Bounded, Enum, Show, Read, Data, Typeable)


-- | Translate `EditDist` flag to a cost function.
costFrom :: EditDist -> String -> A.Cost Char
costFrom Levenshtein _ = A.costDefault
costFrom Specialized x = costSpecial $ length x
costFrom PosMod      x = costPosMod  $ length x


-- | Program arguments.
data CLArgs = CLArgs
    { minPts    :: Int
    , baseEps   :: Double
    , epsMax    :: Double
    , dist      :: EditDist }
    deriving (Data, Typeable, Show)


-- | Epsilon parameter for the given word.
eps :: CLArgs -> String -> Double
eps CLArgs{..} x =
    let n = fromIntegral (length x)
    in  min epsMax (baseEps * n)


-- | Create clustering configuration from program arguments.
configFrom :: CLArgs -> String -> CL.Conf
configFrom cls@CLArgs{..} x = CL.Conf
    { CL.minPts = minPts
    , CL.eps    = eps cls x
    , CL.cost   = costFrom dist x }


clArgs :: CLArgs
clArgs = CLArgs
    { minPts    = 3 &= help "Minimum number of points required to form a cluster"
    , baseEps   = 0.1 &= help
        "A parameter used to identify neighbors of an element"
    , epsMax    = 2.0 &= help
        "A parameter used to identify neighbors of an element"
    , dist      = Levenshtein &= help "Type of edit distance" }
    &= verbosity
    &= summary "Cluster language"
    &= program "cluster-lang"


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgs clArgs


exec :: CLArgs -> IO ()
exec cls = do
    let cfg = configFrom cls
    xs <- map (T.unpack . P.form)
        . P.parsePoliMorf <$> L.getContents
    L.putStrLn $ CL.showClust $ CL.cluster cfg xs
