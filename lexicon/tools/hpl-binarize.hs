import System.Environment (getArgs)
import NLP.Polh.LMF (readPolh)
import NLP.Polh.Binary (savePolh)

main = do
    [lmfPath, binPath] <- getArgs
    savePolh binPath =<< readPolh lmfPath
