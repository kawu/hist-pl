import           System.Environment (getArgs)
import           NLP.HistPL.LMF (readLMF)
import qualified NLP.HistPL as H

main = do
    [lmfPath, binPath] <- getArgs
    H.save binPath =<< readLMF lmfPath
