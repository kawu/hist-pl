{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Control.Monad (forM_)
import Options.Applicative
import Data.Binary (decodeFile)

import qualified Data.Token as Tok
import qualified Text.Tokenize.Simple.String as Tok
-- import qualified Text.Tokenize.Util.String as Tok

import Data.Adict.CostOrd (toCost)
import Data.Adict.Dist (editDist)
import Data.DAWG.Array (size)

import Data.Polh.Types
import Text.Polh.Parse (parsePolh)
import Data.Polh.Collect (HistDict, search, costOrd, pushWord)

data Args = Args
    { polhBinPath   :: FilePath
    , dawgPath      :: FilePath
    , lmfPath       :: FilePath }

argsP :: Parser Args
argsP = Args
    <$> argument str ( metavar "POLH-BINARY" )
    <*> argument str ( metavar "POLI-HIST-DAWG" )
    <*> argument str ( metavar "LMF" )

main :: IO ()
main = execParser opts >>= doCollect
  where
    opts = info (helper <*> argsP)
      ( fullDesc
      & progDesc ( "Collect new word forms from LMF contexts using "
              ++   "POLI-HIST-DAWG to find approximate matchings\n"
              ++ "  and insert them to a POLH-BINARY lexicon." )
      & header "polh-collect-contexts" )

decodeDict :: FilePath -> IO HistDict
decodeDict = decodeFile

doCollect :: Args -> IO ()
doCollect Args{..} = do
    poliHist <- decodeDict dawgPath
    putStr "Dictionary size: "
    print $ size poliHist

    polh <- parsePolh <$> L.readFile lmfPath
    forM_ polh $ \entry -> do
        let contexts =
                [ writtenForm r
                | context <- cxts =<< senses entry
                , r <- repr context, language r == "polh" ]
        forM_ contexts $ \context -> do
            let toks = mapMaybe Tok.unOrth . Tok.tokenize . T.unpack $ context
                lemmas = map T.unpack . text . lemma $ entry
                best = minimumBy (comparing snd)
                    [ (tok, editDist cost baseW tokW)
                    | tok <- toks, let tokW = U.fromList tok
                    , base <- lemmas, let baseW = U.fromList base
                    , let cost = toCost . costOrd . length $ base  ]
            if snd best < 0.4 * average (map length lemmas) && snd best < 3
                then do
                    print ('1', lemmas, best)
                    pushWord polhBinPath "context" (lexId entry)
                        (T.pack $ fst best)
                else print ('2', lemmas, best)
  where
    average xs
        = fromIntegral (sum xs)
        / fromIntegral (length xs)
