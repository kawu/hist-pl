import           System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as L

import           NLP.HistPL.LMF (showLMF)
import qualified NLP.HistPL as H

main = do
    [binPath] <- getArgs
    H.load binPath >>= L.putStr . showLMF
