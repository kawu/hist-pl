import Control.Applicative ((*>), (<*), (<$>))
import Control.Monad (forM_)
import Data.List (intercalate)
import System (getArgs)

import BaseX

-- | Connection configuration.
-- host	= "localhost"
host	= "chopin.ipipan.waw.pl"
port 	= 1984

pushPOS :: Session -> (String, [String]) -> IO ()
pushPOS session (lexId, poss) = do
    putStr lexId
    putStr ": "
    putStrLn $ intercalate " | " poss
    execute session deleteCmd
    forM_ poss push
  where
    push pos = execute session $ insertCmd pos
    deleteCmd
        =  "xquery delete node //LexicalEntry[@id=\"" ++ lexId ++ "\"]"
        ++ "/feat[@att=\"partOfSpeech\"]"
    insertCmd pos
        =  "xquery insert node <feat att=\"partOfSpeech\" "
        ++ "val=\"" ++ pos ++ "\" src=\"automatic:voting\"/> "
        ++ "as first into //LexicalEntry[@id=\"" ++ lexId ++ "\"]"

-- | Read (lexId, POS list) list from STDIN and send to BaseX database. 
main = do
    [user, passwd] <- getArgs
    maybeSession <- connect host port user passwd
    session <- case maybeSession of
        Nothing -> fail "Initializing connection failed"
        Just session -> return session
    execute session "open srpsdp"   -- ^ Set srpsdp context
    
    xs <- map read . lines <$> getContents
    forM_ xs $ pushPOS session
        
    close session
