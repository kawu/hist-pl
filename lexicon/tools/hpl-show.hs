import System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as L

import NLP.HistPL.LMF (showLMF)
import NLP.HistPL.Binary (loadHistPL, lexEntry)

main = do
    [binPath] <- getArgs
    loadHistPL binPath >>= \x -> case x of
        Nothing -> error "Not a dictionary"
        Just ph -> L.putStr (showLMF $ map lexEntry ph)
