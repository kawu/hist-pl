{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | The module provides functionality for manipulating PoliMorf, the
-- morphological dictionary for Polish. Apart from IO utilities there
-- is a 'merge' function which can be used to merge the PoliMorf with
-- another dictionary resources.

module NLP.Polh.Fusion
( 
-- * Rules
  Rule (..)
, apply
, between

-- * Dictionary
, DAWG
, AnaMap
, mkAnaMap
, anaWord
, mkRuleMap
, BaseMap
, mkBaseMap
, FormMap
, mkFormMap

-- * Fusion
, RelCode (..)
, mergeWith
, merge
) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (catMaybes)
import Data.Monoid (mappend)
import Data.Binary (Binary, get, put)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.DAWG.Static as D

import Data.PoliMorf

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

type DAWG a = D.DAWG Char () a

-- | A map from forms to their potential interpretations.  It can be used
-- directly to determine all potential dictionary interpretations of a
-- given form.
-- type AnaMap = DAWG (M.Map Rule (S.Set Tag))
type AnaMap = DAWG (M.Map (Rule, POS) (S.Set MSD))

-- | Construct an 'AnaMap' from a list of entries.
mkAnaMap :: [Entry] -> AnaMap
mkAnaMap xs = D.fromListWith (M.unionWith S.union) $
    [ ( T.unpack (form x)
      , M.singleton
            (between (form x) (base x), pos x)
            (S.singleton (msd x)) )
    | x <- xs ]

-- | Analyse word.
anaWord :: AnaMap -> Form -> M.Map (Base, POS) (S.Set MSD)
anaWord anaMap x = case D.lookup (T.unpack x) anaMap of
    Just m  -> M.fromListWith S.union
        [ ((apply rule x, pos'), msds)
        | ((rule, pos'), msds) <- M.toList m ]
    Nothing -> M.empty

-- | A map from forms to their possible base forms (there may be many since
-- the form may be a member of multiple lexemes).
type BaseMap = DAWG (S.Set (Rule, POS))

-- | A map from base forms to all their potential forms.
type FormMap = DAWG (S.Set (Rule, POS))

elemsOn :: DAWG (S.Set (Rule, POS)) -> String -> [(String, POS)]
elemsOn m x = case x `D.lookup` m of
    Just s  ->
        [ ( T.unpack
          . apply rule
          . T.pack $ x
          , pos' )
        | (rule, pos') <- S.toList s ]
    Nothing -> []

-- | Make a rule map from a list of entries.
mkRuleMap :: [(T.Text, T.Text, POS)] -> DAWG (S.Set (Rule, POS))
mkRuleMap xs = D.fromListWith S.union $
    [ ( T.unpack x
      , S.singleton (between x y, pos') )
    | (x, y, pos') <- xs ]

-- | Make a 'BaseMap' from a list of entries.
mkBaseMap :: [Entry] -> BaseMap
mkBaseMap = mkRuleMap . map ((,,) <$> form <*> base <*> pos)

-- | Make a 'FormMap' from a list of entries.
mkFormMap :: [Entry] -> FormMap
mkFormMap = mkRuleMap . map ((,,) <$> base <*> form <*> pos)

-- | Reliability information: how did we assign a particular label to
-- a particular word form.
data RelCode
    = Exact     -- ^ Label assigned in a direct manner
    | ByBase    -- ^ Label assigned based on a lemma label  
    | ByForm    -- ^ Based on labels of other forms within the same lexeme
    deriving (Eq, Ord, Show, Read)

instance Binary RelCode where
    put Exact   = put '1'
    put ByBase  = put '2'
    put ByForm  = put '3'
    get = get >>= \x -> return $ case x of
        '1' -> Exact
        '2' -> ByBase
        '3' -> ByForm
        c   -> error $ "get: invalid RelCode code '" ++ [c] ++ "'"

-- | Merge the 'BaseMap' with the dictionary resource which maps forms to sets
-- of labels.  Every label is assigned a 'RelCode' which tells what is the
-- relation between the label and the form. It is a generalized version
-- of the 'merge' function with additional function @f x y y'label@ which
-- can be used to determine the resultant set of labels for the form @x@
-- given ,,similar'' form @y@ and its original label @y'label@.
-- There are three kinds of labels:
-- 'Exact' labels assigned in a direct manner, 'ByBase' labels assigned
-- to all forms which have a base form with a label in the input dictionary,
-- and 'ByForm' labels assigned to all forms which have a related form from the
-- same lexeme with a label in the input dictionary.
mergeWith
    :: Ord a
    => (String -> String -> a -> a)
    -> BaseMap
    -> DAWG (S.Set a)
    -> DAWG (M.Map a RelCode)
mergeWith f poli dict0 = D.fromList
    [ (x, combine x)
    | x <- keys ]
  where
    -- Keys in the output dictionary.
    keys = join (D.keys poli) (D.keys dict0)

    -- Combining function.
    combine x = (M.unionsWith min . catMaybes)
        [ label Exact  <$> D.lookup x dict0 
        , label ByBase <$> D.lookup x dict1
        , label ByForm <$> D.lookup x dict2 ]

    label :: Ord a => RelCode -> S.Set a -> M.Map a RelCode
    label code s = M.fromList [(x, code) | x <- S.toList s]

    -- Extended to all base forms of dict0 keys.
    dict1 = D.fromListWith mappend
        [ (lemma, f'Set lemma _form x)
        | (_form, x) <- D.assocs dict0
        , (lemma, _) <- elemsOn poli _form ]

    -- Extended to all forms of dict0 keys.
    dict2 = D.fromListWith mappend
        [ (form', f'Set form' _form x)
        | (_form, x)    <- D.assocs dict0
        , (lemma, pos1) <- elemsOn poli _form
        , (form', pos2) <- elemsOn ilop lemma
        , pos1 == pos2 ]

    -- Inverse poli dictionary.
    ilop = mkRuleMap
        [ (base'Text, form'Text, pos')
        | (form'String, rules) <- D.assocs poli
        , (rule, pos') <- S.toList rules
        , let form'Text = T.pack form'String
        , let base'Text = apply rule form'Text ]
    
    -- Merge to ascending lists.
    join (x:xs) (y:ys)
        | x < y     = x : join xs (y:ys)
        | x > y     = y : join (x:xs) ys
        | otherwise = x : join xs ys
    join xs []  = xs
    join [] ys  = ys

    -- Version of f function working on label sets.
    f'Set v w = S.fromList . map (f v w) . S.toList

-- | A specialized version of the 'mergeWith' function which doesn't
-- change labels in the resultant 'DAWG'.
merge
    :: Ord a => BaseMap
    -> DAWG (S.Set a)
    -> DAWG (M.Map a RelCode)
merge = mergeWith $ \_ _ x -> x
