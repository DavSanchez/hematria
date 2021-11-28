{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Data.IntMap (toDescList)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

data Cipher
  = Simple
  | Custom

data Dictionary
  = Sample
  | Spanish
  | English

newtype CipherData = CipherData
  { cipher :: M.Map Char Int
  }
  deriving (Show)

newtype DictionaryData = Dictionary
  { dict :: IM.IntMap (S.Set T.Text)
  }
  deriving (Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- |Analyzes a word with the cipher and returns its numerical value 
-- and a list of words in the dictionary with the same value
gematria :: CipherData -> DictionaryData -> T.Text -> (Int, Maybe [T.Text])
gematria c (Dictionary d) w = (v, S.toDescList <$> IM.lookup v d)
  where
    v = computeNumericalValue c w

-- |Gets the cipher from the selected from the available ones
-- >>> getCipher Simple
-- CipherData {cipher = fromList [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7),('h',8),('i',9),('j',10),('k',11),('l',12),('m',13),('n',14),('o',16),('p',17),('q',18),('r',19),('s',20),('t',21),('u',22),('v',23),('w',24),('x',25),('y',26),('z',27),('\241',15)]}
getCipher :: Cipher -> CipherData
getCipher Simple = simpleCipher
getCipher _ = undefined

-- |Generates a dictionary from a cipher and a word list
-- >>> getDictionary Simple sampleWordList
-- Dictionary {dict = fromList [(65,fromList ["Ritual"]),(76,fromList ["Brujer\237a"]),(83,fromList ["Lujuria"]),(88,fromList ["Hechizos"]),(98,fromList ["Viaje Astral"]),(117,fromList ["Conjuros"])]}
getDictionary :: Cipher -> [T.Text] -> DictionaryData
getDictionary c = buildDictionary $ getCipher c

simpleCipher :: CipherData
simpleCipher = CipherData $ M.fromList $ zip ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'ñ', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'] [1 ..]

sampleWordList :: [T.Text]
sampleWordList = ["Brujería", "Viaje Astral", "Hechizos", "Conjuros", "Lujuria", "Ritual"]

-- |Builds a dictionary from the cipher data and a list of words
buildDictionary :: CipherData -> [T.Text] -> DictionaryData
buildDictionary c wl = Dictionary $ IM.fromListWith (<>) $ map (wordWithValue c) wl

-- |Computes the numerical value of a word using the cipher
-- >>> computeNumericalValue simpleCipher "Ritual"
-- 65
computeNumericalValue :: CipherData -> T.Text -> Int
computeNumericalValue c w = sum $ map (\char -> M.findWithDefault 0 char (cipher c)) (T.unpack w)

-- |Computes the numerical value of a word using the cipher, returning 
-- a tuple with the numerical value and the word in a singleton set
-- for compilation in a dictionary
-- >>> wordWithValue simpleCipher "Ritual"
-- (65,fromList ["Ritual"])
wordWithValue :: CipherData -> T.Text -> (Int, S.Set T.Text)
wordWithValue c w = (computeNumericalValue c w, S.singleton w)
