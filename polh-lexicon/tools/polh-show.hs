import System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as L

import Text.Polh.Show (showPolh)
import Data.Polh.IO (loadPolh)

main = do
    [binPath] <- getArgs
    polh <- loadPolh binPath
    L.putStr (showPolh polh)
