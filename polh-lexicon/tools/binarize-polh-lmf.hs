import Control.Applicative ((<$>))
import System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as L

import Text.Polh.Parse (parsePolh)
import Data.Polh.IO (savePolh)

main = do
    [lmfPath, binPath] <- getArgs
    polh <- parsePolh <$> L.readFile lmfPath
    savePolh binPath polh
