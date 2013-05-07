import System.Environment (getArgs)
import NLP.HistPL.LMF (readLMF)
import NLP.HistPL.Binary (saveHistPL)

main = do
    [lmfPath, binPath] <- getArgs
    saveHistPL binPath =<< readLMF lmfPath
