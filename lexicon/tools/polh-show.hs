import System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as L

import NLP.Polh.LMF (showPolh)
import NLP.Polh.Binary (loadPolh)

main = do
    [binPath] <- getArgs
    polh <- loadPolh binPath
    L.putStr (showPolh polh)
