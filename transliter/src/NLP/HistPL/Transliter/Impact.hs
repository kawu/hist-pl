module NLP.HistPL.Transliter.Impact
( rules
) where

import Control.Applicative ((<$>), (<*))
import Text.Parsec hiding (Line)

import NLP.HistPL.Transliter hiding (wordRules, charRules)

-- | A set of transliteration rules prepared for documents
-- from the IMPACT project. 
rules :: TrRules
rules = TrRules wordRules charRules

-- Word-level transliteration rules 
wordRules :: [Parser String]
wordRules = 
    [ "y" #> "i"
    , "ztąd" #> "stąd"
    , "i" #> "j" >+> vowel >+> many1 anyChar
    , "ktor" #> "któr" >+> many1 anyChar
    , "gor" #> "gór" >+> many1 anyChar
    , "wtor" #> "wtór" >+> many1 anyChar
    , "rożn" #> "różn" >+> many1 anyChar
    , ("anyol" .| "angol" .|. "angyol" .|. "angiol" >#> "anjoł")
        >+> many anyChar ]

-- Character-level transliteration rules 
charRules :: [Parser String]
charRules =
    [ "à" .|  "á" .|. "á" .|. "â" .|. "ã" .|. "ä" .|. "ȧ" >#> "a"
    , "è" .|  "é" .|. "ê" .|. "ë" .|. "ē" .|. "ĕ"
          .|. "ė" .|. "ẽ" .|. "ə" >#> "e"
    , "ì" .|  "í" .|. "î" .|. "ï" >#> "i"
    , "ñ" #> "ń"
    , "z̄" #> "ż"
    , "ċ" #> "ć"
    , "ṡ" #> "ś"
    , "ow" #> "ów" <* eof
    , "æ" .| "œ" >#> "e"
    , "ⱥ" #> "ą"
    , "ɇ" .| "ẹ" >#> "ę"
    , "ø" #> "ą" -- lub ę
    , sChar >#> "s"   >+> (vowel <|> hardConsonant)
    , sChar >#> "ś"   >+> softConsonant
    , sChar >#> "s"   >+> str "k"
    -- , sChar >#> "s"   >+> str "ki"
    , "ʃʃ" .| "ſſ" .|. "ſs" .|. "ſz" .|. "β" .|. "ß" >#> "sz"
    , "\60122" #> "st"
    , "\60322" #> "si"
    , "ﬆ" #> "st"   -- czy to dobra reguła? 
    , "źi" .| "żi" >#> "zi"
    , "ći" .| "ċi" >#> "ci"
    , "ńi" .| "ṅi" >#> "ni"
    , "śi" #> "si"
    , "dźi" #> "dzi"
    , "cż" #> "cz"
    , "rż" #> "rz"
    , "sż" #> "sz"
    , str "e" >+> ("x" #> "gz")
    , "x" #> "ksz" >+> str "t"
    , "x" #> "ks"
    , (str "t" <|> str "p" <|> str "r") >+> ("h" #> "")
    , "&" #> "et"
    -- , vowel >+> ("y" #> "j") >+> consonant
    , vowel >+> ("y" #> "j")
    , vowel >+> ("i" #> "j") >+> vowel
    , hardConsonant >+> ("y" #> "yj") >+> vowel
    , softConsonant >+> ("y" #> "ij") >+> vowel
    , "ss" #> "s"
    , "szsz" #> "sz"
    , "mm" #> "m"
    , "ff" #> "f"
    , "pp" #> "p"
    , "ll" #> "l"
    , "tt" #> "t"
    , "v" #> "w" >+> vowel
    , "v" #> "u"
    , "ts" #> "c"
    , "ds" #> "dz"
    , "łi" #> "li"
    , "srz" #> "śrz"
    , "zrz" #> "źrz"
    , singleton <$> anyChar ] -- default
  where
    str = ciString
    sChar = "ʃ" .| "ſ" .|. "\60326" 

-- | FIXME: Samogłoski powinny obejmować także znaki historyczne?
vowel :: Parser String
vowel = singleton <$> oneOf "aąeęioóuy"

softConsonant :: Parser String
softConsonant = ciString "dź"
            <|> (singleton <$> oneOf "bmflśźńć") 

hardConsonant :: Parser String
hardConsonant = (singleton <$> oneOf "pwżcsztnrł")
            <|> (ciString "d" <* notFollowedBy (ciChar 'ź'))

-- consonant :: Parser String
-- consonant = singleton <$> oneOf "bcćdfghjklłmnńprsśtwzźż"

singleton :: a -> [a]
singleton = (:[])
