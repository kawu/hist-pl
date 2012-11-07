import System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as L

import NLP.Polh.LMF (showPolh)
import NLP.Polh.Binary (loadPolh)

main = do
    [binPath] <- getArgs
    loadPolh binPath >>= \x -> case x of
        Nothing -> error "Not a dictionary"
        Just ph -> L.putStr (showPolh ph)
