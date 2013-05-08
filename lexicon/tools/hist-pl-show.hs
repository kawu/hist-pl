import           System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as L

import           NLP.HistPL.LMF (showLMF)
import qualified NLP.HistPL.Binary as H

main = do
    [binPath] <- getArgs
    H.load binPath >>= \x -> case x of
        Nothing -> error "Not a dictionary"
        Just pl -> L.putStr (showLMF $ map H.lexEntry pl)
