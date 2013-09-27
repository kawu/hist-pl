module NLP.HistPL.Similar
( lookupSim
) where


import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import qualified NLP.Adict as A
import           NLP.HistPL.DAWG
import           NLP.HistPL.Lexicon
import           NLP.HistPL.Similar.Cost


-- | Perform approximate searching in the dictionary with a given
-- similarity threshold.  Only the most similar match will be considered.
lookupSim :: HistPL -> T.Text -> Double -> Maybe (T.Text, Double)
lookupSim hpl x th = lookupApprox x th (formMap hpl)


-- | Approximate lookup in the dictionary automaton.
lookupApprox :: Ord i => T.Text -> Double -> DAWG i a b -> Maybe (T.Text, Double)
lookupApprox x th dict =
    case A.findAll (costSpecial $ T.length x) th (V.fromList $ T.unpack x) dict of
        ((y, _, w) :_)  -> Just (T.pack y, w)
        []              -> Nothing
