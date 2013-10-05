module NLP.HistPL.Poser.Entropy
( entropy
, sampleEntropy
) where

-- | Entropy of given distribution.
entropy :: [Double] -> Double
entropy ps = - sum
    [ px * log px
    | px <- filter (>0) ps ]

-- | Simple entropy estimator using pseudocounts.
sampleEntropy :: [Double] -> Double
sampleEntropy = entropy . normalize . (++) (replicate 2 1.0)

normalize :: [Double] -> [Double]
normalize xs = map (/sum xs) xs
