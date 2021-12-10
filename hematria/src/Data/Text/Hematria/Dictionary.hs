{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Hematria.Dictionary where

import Cache (getDictFromCache, listCachedDicts)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Hematria.Cipher (Cipher, computeNumericalValue, getCharValue)
import qualified Data.Text.IO as TextIO
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Data.Char (isAlpha)

data Dictionary
  = Spanish
  | English
  deriving (Eq, Show, FromJSON, Generic)

newtype DictionaryData = DictData
  { dict :: IM.IntMap (S.Set T.Text)
  }
  deriving (Eq, Show)

-- | Gets the cipher from the selected from the available ones
--  >>> getCipher Sample
getDictionary :: Dictionary -> IO [T.Text]
getDictionary Spanish = sanitize <$> getDictFromCache "spanish"
getDictionary English = sanitize <$> getDictFromCache "english"

sanitize :: [T.Text] -> [T.Text]
sanitize = map (T.toTitle . T.strip) . filter (/= T.empty)

getCipheredDictionary :: Cipher -> Dictionary -> IO DictionaryData
getCipheredDictionary c d = buildDictionary c <$> getDictionary d

-- | Builds a dictionary from the cipher data and a list of words, removing the ones with value 0
buildDictionary :: Cipher -> [T.Text] -> DictionaryData
buildDictionary c wl = DictData $ IM.delete 0 $ IM.fromListWith (<>) $ map (wordWithValue c) wl

-- | Computes the numerical value of a word using the cipher, returning
--  a tuple with the numerical value and the word in a singleton set
--  for compilation in a dictionary
--  >>> wordWithValue SpanishSimple "Ritual"
--  (65,fromList ["Ritual"])
wordWithValue :: Cipher -> T.Text -> (Int, S.Set T.Text)
wordWithValue c w = (computeNumericalValue c w, S.singleton w)

-- getCipheredWords :: DictionaryData -> Int -> Maybe [T.Text]
getCipheredWords :: DictionaryData -> Int -> Maybe (S.Set T.Text)
getCipheredWords d v = IM.lookup v (dict d)

listDicts :: IO ()
listDicts = do
  dicts <- listCachedDicts
  TextIO.putStrLn "Available dictionaries:\n"
  TextIO.putStrLn "\t- sample (sample words, mainly for testing)" -- Comment out for release?
  mapM_ (\d -> TextIO.putStrLn ("\t- " <> d)) dicts