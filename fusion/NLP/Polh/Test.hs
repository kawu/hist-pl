{-# LANGUAGE RecordWildCards #-}

module NLP.Polh.Test
(
-- * Rules
  Rule (..)
, apply
, between
) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, get, put)
import Data.Text.Binary ()
import Data.PoliMorf (POS)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.DAWG.Static as D

-- | A rule for translating a form into another one.
data Rule = Rule {
    -- | Number of characters to cut from the end of the form.
      cut       :: !Int
    -- | A suffix to paste.
    , suffix    :: !T.Text }
    deriving (Show, Eq, Ord)

instance Binary Rule where
    put Rule{..} = put cut >> put suffix
    get = Rule <$> get <*> get

-- | Apply the rule.
apply :: Rule -> T.Text -> T.Text
apply r x = T.take (T.length x - cut r) x `T.append` suffix r

-- | Make a rule to translate between two strings.
between :: T.Text -> T.Text -> Rule
between source dest =
    let k = lcp source dest
    in  Rule (T.length source - k) (T.drop k dest)
  where
    lcp a b = case T.commonPrefixes a b of
        Just (c, _, _)  -> T.length c
        Nothing         -> 0

------------------------------------------------------------------------

-- | Basic types.
type UID = Int
type Word = T.Text
type Base = T.Text
type IsBase = Bool

-- | DAWG with `String` keys.
type DAWG a = D.DAWG Char () a

-- | One-way dictionary parametrized over ID @i@, with info @a@ for every
-- (key, i) pair and info @b@ for every (key, i, apply rule key) triple.
type Dict i a b = DAWG (M.Map i (M.Map Rule b, a))
-- type Dict = DAWG (M.Map POS (S.Set Rule))

-- LexID i = (Base, i)?

mkDict :: (Ord i, Ord a, Ord b) => [(T.Text, i, a, T.Text, b)] -> Dict i a b
mkDict xs = D.fromListWith union $
    [ ( T.unpack x
      , M.singleton i
        (M.singleton (between x y) b, a) )
    | (x, i, a, y, b) <- xs ]
  where
    union = M.unionWith $ both M.union const
    both f g (x, y) (x', y') = (f x x', g y y')

-- Dictionary keys include base forms and rules transform base forms to
-- their corresponding word forms.  Info @a@ is assigned to every lexeme
-- and info @b@ to every word form.
type BaseDict i a b = Dict i a b

-- Dictionary keys include word forms and rules transform word forms to
-- their corresponding base forms.  Info @a@ is assigned to every word
-- form.
type FormDict i a = Dict i () a

-- | Bilateral dictionary.
data Bila i a b = Bila
    { baseDict  :: BaseDict i a b
    , formDict  :: FormDict i b }
    deriving (Show, Eq, Ord)

-- | Historical dictionary.
type Hist = Dict UID (S.Set POS) IsBase

-- | Entry from historical dictionary.
data HLex = HLex
    { hUID      :: UID
    , hPOSs     :: S.Set POS
    , hWords    :: M.Map Word IsBase }
    deriving (Show, Eq, Ord)

-- | List all lexical entries from historical dictionary.
histLexs :: Hist -> [HLex]
histLexs hist =
    [ HLex uid poss $ M.fromList
        [ (apply rule key'Text, isBase)
        | (rule, isBase)    <- M.assocs rules ]
    | (key, idMap)          <- D.assocs hist
    , let key'Text          =  T.pack key
    , (uid, (rules, poss))  <- M.assocs idMap ]

type LexID = (Base, POS)

type LexSet = M.Map LexID (S.Set Word)

type Corresp a b = Bila POS a b -> HLex -> LexSet

buildCorresp
    :: (Bila POS a b -> HLex -> [LexSet])   -- ^ Core function
    -> (LexID -> S.Set Word -> Bool)        -- ^ Filter
    -> ([LexSet] -> LexSet)                 -- ^ Choice
    -> Corresp a b
buildCorresp core filt choice bila
    = choice
    . map filterSet
    . core bila
  where
    filterSet :: LexSet -> LexSet
    filterSet
        = M.fromList
        . filter (uncurry filt)
        . M.assocs

extend :: HLex -> LexSet -> HLex
extend = undefined

data Code
    = Orig
    | Copied
    deriving (Show, Eq, Ord)

-- fuse :: Corresp a b -> Hist -> Bila POS a b -> Dict UID () Code
-- fuse corr hist bila = D.fromList
--     [ let HLex{..} = extend hlex (corr bila lex)
--       in  
--     | lex <- histLexs hist ]
