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
import           Data.List (intercalate)
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
appInit binPath = makeSnaplet "hist-pl" "HistPL" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit "templates"
    modifyHeistState $ bindSplices
        [ ("lex-entry", lexSplice)
        , ("ana-result", anaSplice) ]
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


-- | Show information about lexemes specified by an identifier or a form.
lexSplice :: Splice AppH
lexSplice = failLeft . runEitherT $ lexByID <|> lexByForm


-- | Show information about lexemes specified by a lexeme identifier.
lexByID :: EitherT [String] (HeistT AppH AppH) Template
lexByID = do
    hpl   <- gets _histPL
    lexID <- lift (getParam "id") >>= tryJust ["Param @id not specified"]
    entry <- liftIO (H.tryLoad' hpl $ T.decodeUtf8 lexID) >>=
        tryJust ["No etries with given @id"]
    hoistEither $ lexToHTML entry


-- | Show information about lexemes specified by a form.
lexByForm :: EitherT [String] (HeistT AppH AppH) Template
lexByForm = do
    hpl <- gets _histPL
    form <- lift (getParam "form") >>= tryJust ["Param @form not specified"]
    entries <- liftIO $ H.lookup hpl $ T.decodeUtf8 form
    hoistEither $ decorate entries
  where
    decorate xs = do
        let h = numInfo xs
        nodes <- concat <$> mapM decorateEntry xs
        return $ h : nodes
    numInfo xs =
        let n = T.pack $ show $ length xs
            info = "Number of interpretations: " `T.append` n
        in  X.Element "h3" [] [X.TextNode info]
    decorateEntry (entry, code) = do
        desc <- lexToHTML entry
        let h = case code of
                H.Copy  -> "Possible interpretation (based on PoliMorf):"
                _       -> "Historical interpretation:"
        return $ X.Element "h5" [] [X.TextNode h] : desc


-- | Fail on left value.
failLeft :: Monad m => m (Either [String] b) -> m b
failLeft m = m >>= \x -> case x of
    Left es -> fail (intercalate ", " es)
    Right y -> return y


-- | Translate entry to a Heist template (a list of HTML nodes).
lexToHTML :: H.LexEntry -> Either [String] Template
lexToHTML entry
    = both (:[]) X.docContent
    $ X.parseHTML "-" (decorate entry)
  where
    both f _ (Left x)   = Left  (f x)
    both _ g (Right x)  = Right (g x)
    decorate = B.fromString
        . Pandoc.writeHtmlString Pandoc.def
        . Pandoc.readMarkdown Pandoc.def
        . (\txt -> "~~~~~ {.xml}\n" ++ txt ++ "\n~~~~~\n")
        . L.unpack . H.showLexEntry


-- | Factorial splice.
factSplice :: Splice AppH
factSplice = do
    input <- getParamNode
    let text = T.unpack $ X.nodeText input
        n = read text :: Int
    return [X.TextNode $ T.pack $ show $ product [1..n]]


-- | Analysis splice.
anaSplice :: Splice AppH
anaSplice = 
    return [X.TextNode "HAHAHA"]


----------------------------------
-- Server
----------------------------------


main :: IO ()
main = do
    [binPath] <- getArgs
    serveSnaplet defaultConfig $ appInit binPath
