{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Lens (makeLenses)
import           System.Environment (getArgs)
import           Control.Error
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe (serveDirectory)
import           Heist
import           Heist.Interpreted hiding (textSplice)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.UTF8 as B
import qualified Text.XmlHtml as X
import qualified Text.Pandoc as Pandoc

import qualified NLP.HistPL.Lexicon as H
import qualified NLP.HistPL.LMF as H


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
    modifyHeistState $ bindSplices
        [ ("lex-entry", lexSplice) ]
    hp <- liftIO $ H.open binPath
    addRoutes [ ("hello", writeText "hello world")
              , ("echo", echoHandler)
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
lexSplice :: Splice AppH
lexSplice = report . runEitherT $ do
    hpl   <- lift   $ gets _histPL
    lexID <- lift (getParam "id") >>=
        tryJust "Param @id not specified"
    entry <- liftIO (H.tryLoad' hpl $ T.decodeUtf8 lexID) >>=
        tryJust "No etry with given @id"
    hoistEither $ X.parseHTML "-" (decorate entry)
  where
    decorate = B.fromString
             . Pandoc.writeHtmlString Pandoc.def
             . Pandoc.readMarkdown Pandoc.def
             . (\txt -> "~~~~~ {.xml}\n" ++ txt ++ "\n~~~~~\n")
             . L.unpack . H.showLexEntry
    report app = app >>= \x -> case x of
        Right y -> return $ X.docContent y
        Left e  -> fail e




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
