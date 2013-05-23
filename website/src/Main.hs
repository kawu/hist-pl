{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where


import           System.Environment (getArgs)
import           Control.Lens (makeLenses)
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe (serveDirectory)
import           Heist
import           Heist.Interpreted hiding (textSplice)
import qualified Data.Text as T
import qualified Text.XmlHtml as X

import qualified NLP.HistPL.Lexicon as H


----------------------------------
-- Configuration
----------------------------------

-- TODO

----------------------------------
-- Application snaplet
----------------------------------


-- | Application state.
data App = App
    { _heist    :: Snaplet (Heist App)
    , _histPL   :: H.HistPL }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


-- | Application handler monad.
type AppH = Handler App App


-- | Application initialization.  You need to supply path to the
-- binary dictionary.
appInit :: FilePath -> SnapletInit App App
appInit binPath = makeSnaplet "myapp" "My example application" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit "templates"
    hp <- liftIO $ H.open binPath
    -- addSplices [ ("fact", factSplice) ]
    addRoutes [ ("hello", writeText "hello world")
              , ("echo", echoHandler)
              , ("lex", lexHandler)
              , ("public", serveDirectory "resources/public")
              , ("", heistServe) ]
    return $ App hs hp


----------------------------------
-- Splices and handlers
----------------------------------


-- | Echoes @echoparam@ parameter.
echoHandler :: AppH ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL") writeBS param


-- | Show various information concerning the lexeme specified by the
-- @lexid@ parameter.
lexHandler :: AppH ()
lexHandler = do
    lexID <- getParam "id"
    maybe (return ()) writeBS lexID


-- | Factorial splice.
factSplice :: Splice AppH
factSplice = do
    input <- getParamNode
    let text = T.unpack $ X.nodeText input
        n = read text :: Int
    return [X.TextNode $ T.pack $ show $ product [1..n]]


----------------------------------
-- Server
----------------------------------


main :: IO ()
main = do
    [binPath] <- getArgs
    serveSnaplet defaultConfig $ appInit binPath
