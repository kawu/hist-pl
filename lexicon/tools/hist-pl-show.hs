import           System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as L

import           NLP.HistPL.LMF (showLMF)
import qualified NLP.HistPL as H

main = do
    [binPath] <- getArgs
    hpl <- H.open binPath
    H.load hpl >>= L.putStr . showLMF . map snd
