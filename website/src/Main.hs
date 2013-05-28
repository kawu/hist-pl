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
import qualified NLP.HistPL.Analyse as A


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
        , ("ana-input", anaInpSplice)
        , ("ana-output", anaOutSplice) ]
    hp <- liftIO $ H.open binPath
    addRoutes [ ("hello", writeText "hello world")
              , ("echo", echoHandler)
              , ("public", serveDirectory "resources/public")
              , ("", heistServe) ]
    return $ App hs hp


----------------------------------
-- Search handlers and splices
----------------------------------


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
    form <- T.decodeUtf8 <$> ( lift (getParam "form")
        >>= tryJust ["Param @form not specified"] )
    entries <- liftIO $ H.lookupMany hpl [form, T.toLower form]
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


----------------------------------
-- Analysis handlers and splices
----------------------------------


-- | Analysis input splice.
anaInpSplice :: Splice AppH
anaInpSplice = do
    input <- maybe "" id <$> getPostParam "input"
    return [X.TextNode $ T.decodeUtf8 input]


-- | Analysis output splice.
anaOutSplice :: Splice AppH
anaOutSplice = do
    hpl <- gets _histPL
    raw <- maybe "" id <$> getPostParam "input"
    let input = T.filter (/='\r') (T.decodeUtf8 raw)
        plug  = [X.Element "br" [] [], X.TextNode " "]
    intercalate plug <$> mapM (anaLine hpl) (T.lines input)
  where
    anaLine hpl line = mapM (anaTok hpl) (A.tokenize line)
    anaTok _ (Right o)  = return $
        let n = X.TextNode (showOther o)
        in  X.Element "code" [] [n]
    anaTok hpl (Left x) = do
        t <- liftIO $ A.anaWord hpl x
        let n = X.Element "code" [] [X.TextNode x]
        return $ if hasHist t
            then addLink x (showTip t) n
            else n
    addLink x tip n =
        let xdiv = X.Element "span" [("class", "fl"), ("title", tip)]
            href = X.Element "a" [("href", "../lex?form=" `T.append` x)]
        in  xdiv [href [n]]
    showOther (A.Pun x)   = x
    showOther (A.Space x) = x

    -- Does the token have any obvious historical interpretation?
    -- hasEvidentHist tok = isJust $ find ((/=) H.Copy . snd) (A.hist tok)

    -- Does the token have any historical interpretation?
    hasHist tok = length (A.hist tok) > 0

    -- Show tooltip with definitions for each potential historical
    -- interpretation of the token.
    showTip tok = T.intercalate "\n"
        [ T.append
            (wrapBase $ baseForms entry)
            (showDefs $ lexDefs entry)
        | entry <- map fst $ A.hist tok ]
    baseForms  = H.text . H.lemma
    wrapBase x = "<<" `T.append` T.intercalate ", " x `T.append` ">>"
    showDefs   = T.concat . map (T.append "\n * " . T.intercalate ", ")


-- | Get a list of lexeme definitions.
lexDefs :: H.LexEntry -> [[T.Text]]
lexDefs entry =
    [ concat [H.text x | x <- H.defs sense]
    | sense <- H.senses entry ]


----------------------------------
-- Examples
----------------------------------


-- | Factorial splice.
factSplice :: Splice AppH
factSplice = do
    input <- getParamNode
    let text = T.unpack $ X.nodeText input
        n = read text :: Int
    return [X.TextNode $ T.pack $ show $ product [1..n]]


-- | Echoes @echoparam@ parameter.
echoHandler :: AppH ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL") writeBS param


----------------------------------
-- Server
----------------------------------


main :: IO ()
main = do
    binPath : _ <- getArgs
    serveSnaplet defaultConfig $ appInit binPath
