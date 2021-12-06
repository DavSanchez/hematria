module Data.Text.Hematria.Dictionary where

import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Hematria.Cipher (Cipher, getCharValue)
import Data.Text.Hematria.Dictionary.Sample (sampleWordList)
import Data.Text.Hematria.Dictionary.Spanish (spanishWordList)

data Dictionary
  = Sample
  | Spanish
  | English
  | Custom
  deriving (Show)

newtype DictionaryData = DictData
  { dict :: IM.IntMap (S.Set T.Text)
  }
  deriving (Show)

-- | Gets the cipher from the selected from the available ones
--  >>> getCipher Sample
getDictionary :: Dictionary -> [T.Text]
getDictionary Sample = sampleWordList
getDictionary Spanish = spanishWordList
getDictionary _ = undefined

getCipheredDictionary :: Cipher -> Dictionary -> DictionaryData
getCipheredDictionary c d = buildDictionary c $ getDictionary d

-- | Builds a dictionary from the cipher data and a list of words
buildDictionary :: Cipher -> [T.Text] -> DictionaryData
buildDictionary c wl = DictData $ IM.fromListWith (<>) $ map (wordWithValue c) wl

-- | Computes the numerical value of a word using the cipher
--  >>> computeNumericalValue simpleCipher "Ritual"
--  65
computeNumericalValue :: Cipher -> T.Text -> Int
computeNumericalValue c w = sum $ map (getCharValue c) (T.unpack w)

-- | Computes the numerical value of a word using the cipher, returning
--  a tuple with the numerical value and the word in a singleton set
--  for compilation in a dictionary
--  >>> wordWithValue SpanishSimple "Ritual"
--  (65,fromList ["Ritual"])
wordWithValue :: Cipher -> T.Text -> (Int, S.Set T.Text)
wordWithValue c w = (computeNumericalValue c w, S.singleton w)

getCipheredWords :: Cipher -> Dictionary -> Int -> Maybe [T.Text]
getCipheredWords c d v = S.toAscList <$> IM.lookup v (dict $ getCipheredDictionary c d)