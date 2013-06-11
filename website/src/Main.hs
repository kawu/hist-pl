{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main where


import           Control.Lens (makeLenses)
import           System.Environment (getArgs)
import           Control.Error
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe (serveDirectory)
import           Heist
import           Heist.Interpreted hiding (textSplice)
import qualified Data.Map as M
import           Data.List (intercalate, intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.XmlHtml as X

import qualified NLP.HistPL.Lexicon as H
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
        , ("ana-output", anaOutSplice)
        , ("list-output", listOutSplice) ]
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
lexSplice = ignoreLeft . runEitherT $ lexByID <|> lexByForm


-- | Show information about lexemes specified by a lexeme identifier.
lexByID :: EitherT [String] (HeistT AppH AppH) Template
lexByID = do
    hpl   <- gets _histPL
    lexID <- lift (getParam "id") >>= tryJust ["Param @id not specified"]
    entry <- liftIO (H.tryLoadI hpl $ T.decodeUtf8 lexID) >>=
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
    decorate xs = intercalate hr <$> mapM (lexToHTML . fst) xs
    hr = [X.Element "hr" [] []]

--     numInfo xs =
--         let n = T.pack $ show $ length xs
--             info = "Number of interpretations: " `T.append` n
--         in  X.Element "h3" [] [X.TextNode info]
--     decorateEntry (entry, _code) = do
--         desc <- lexToHTML entry
--         let h = case code of
--                 H.Copy  -> "Possible interpretation (based on PoliMorf):"
--                 _       -> "Historical interpretation:"
--         return $ X.Element "h5" [] [X.TextNode h] : desc


-- -- | Translate entry to a Heist template (a list of HTML nodes).
-- lexToHTML :: H.LexEntry -> Either [String] Template
-- lexToHTML entry
--     = both (:[]) X.docContent
--     $ X.parseHTML "-" (decorate entry)
--   where
--     both f _ (Left x)   = Left  (f x)
--     both _ g (Right x)  = Right (g x)
--     decorate = B.fromString
--         . Pandoc.writeHtmlString Pandoc.def
--         . Pandoc.readMarkdown Pandoc.def
--         . (\txt -> "~~~~~ {.xml}\n" ++ txt ++ "\n~~~~~\n")
--         . L.unpack . H.showLexEntry


-- | Translate entry to a Heist template (a list of HTML nodes).
lexToHTML :: H.LexEntry -> Either [String] Template
lexToHTML entry = Right
    [ header
    , forms
    , senses ]
  where

    -- Header with base forms
    header = X.Element "h2" [] $ wrapBase ++ [space] ++ wrapPos
    wrapBase  = commas $ H.text $ H.lemma entry
    wrapPos   = parens $ commas $ H.pos entry

    -- Section with forms
    forms = X.Element "div" [("class", "lex-forms")] $ formsHead : [formsBody]
    formsHead = mkH "h3" "Formy gramatyczne"
    formsBody
        | null defs = X.Element "i" [] [X.TextNode "Brak"]
        | otherwise = X.Element "span" [("class", "lex-forms-body")]
            $ commas defs
      where
        defs = concatMap H.text $ H.forms entry

    -- Section with senses
    senses = X.Element "div" [("class", "lex-senses")]
        $ sensesHeader : sensesBody
    sensesHeader = mkH "h3" "Znaczenia"
    sensesBody 
        | null xs   = [X.Element "i" [] [X.TextNode "Brak"]]
        | otherwise = map (uncurry mkSense) $ zip [1 :: Int ..] xs
        where xs = H.senses entry

    -- Section with one sense
    mkSense i x = X.Element "div" [("class", "lex-sense")]
        [ senseHeader i x, senseBody x ]
    senseHeader i x = X.Element "h4" []
        $ X.TextNode (T.pack $ show i ++ ". ")
        : commas (concatMap H.text $ H.defs x)
    senseBody x = X.Element "ul" [] $
        map (X.Element "li" [] . (:[]) . X.TextNode)
            (concatMap H.text $ H.cxts x)

    -- Utilities
    commas    = intersperse (X.TextNode ", ") . map X.TextNode
    parens xs = X.TextNode "(" : xs ++ [X.TextNode ")"]
    space     = X.TextNode " "
    mkH h x   = X.Element h [] [X.TextNode x]


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
-- Index handlers and splices
----------------------------------


-- | List subpage parameters. 
data LParams = LParams
    { pref  :: T.Text
    , beg   :: Int
    , form  :: T.Text }


-- | Parse query parameters.
parseLParams :: Monad m => Params -> EitherT [String] m LParams
parseLParams params = do
    beg <- case lookupParam "beg" params of
        Nothing -> return 0
        Just x  -> tryRead ["Param @beg not a number"] (T.unpack $ T.decodeUtf8 x)
    let pref = maybe "" T.decodeUtf8 $ lookupParam "prefix" params
        form = maybe "" T.decodeUtf8 $ lookupParam "form" params
    return $ LParams
        { pref  = pref
        , beg   = beg
        , form  = form }


-- | Show `LParams`.
showLParams :: LParams -> T.Text
showLParams LParams{..}
    = T.cons '?' $ T.intercalate "&"
    $ map (\(x, y) -> x `T.append` "=" `T.append` y)
        [ ("prefix", pref)
        , ("beg",  T.pack $ show beg)
        , ("form", form) ]


-- | List splice prints all entries with a specified prefix.
listOutSplice :: Splice AppH
listOutSplice = failLeft $ runEitherT $ do
    params <- parseLParams =<< lift getQueryParams
    hpl <- gets _histPL
    return $ listTempl hpl params


listTempl :: H.HistPL -> LParams -> Template
listTempl hpl lp@LParams{..} =

    [ X.TextNode $ "Liczba znalezionych form: " `T.append` T.pack (show n)
    , prevNext
    , X.Element "ul" [] $ map mkItem $ catMaybes
        [ T.append pref <$> H.nthSuffix hpl pref i
        | i <- [beg .. min n (beg + showNum) - 1] ]
    , prevNext ]

  where

    -- Number of entries with the prefix.
    n = H.withPrefix hpl pref

    -- How many forms will be shown on one page.
    showNum = 100

    -- Make a list element from a form.
    mkItem x = X.Element "li" [("class", "fl")]
        [linkHere x $ \p -> p {form = x}]

    -- Left/right links.
    prevNext = X.Element "div" [("id", "prev-next")]
        $ intersperse (X.TextNode " | ")
        $ prev ++ next

    -- Print "Poprzednie" link, if needed.
    prev | beg > 0 = [linkHere "Poprzednie" $ shiftBeg (-showNum)]
         | otherwise = []

    -- Print "Następne" link, if needed.
    next | beg + showNum < n = [linkHere "Następne" $ shiftBeg showNum]
         | otherwise = []

    -- Shift @beg URI parameter.
    shiftBeg k p = p { beg = max (beg + k) 0 }

    -- Link to this page with changed parameters.
    linkHere x f = X.Element "a"
        [("href", showLParams (f lp))]
        [X.TextNode x]


----------------------------------
-- Handling error value
----------------------------------


-- | Fail on left value.
failLeft :: Monad m => m (Either [String] b) -> m b
failLeft m = m >>= \x -> case x of
    Left es -> fail (intercalate ", " es)
    Right y -> return y


-- | Fail on left value.
ignoreLeft :: Monad m => m (Either [String] Template) -> m Template
ignoreLeft m = m >>= \x -> return $ case x of
    Left _  -> []
    Right y -> y


----------------------------------
-- Misc
----------------------------------


lookupParams :: Ord a => a -> M.Map a [b] -> [b]
lookupParams x = maybe [] id . M.lookup x


lookupParam :: Ord a => a -> M.Map a [b] -> Maybe b
lookupParam x = listToMaybe . lookupParams x


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
