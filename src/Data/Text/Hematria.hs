module Data.Text.Hematria where

import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Hematria.Cipher (Cipher, getCharValue)
import Data.Text.Hematria.Dictionary
  ( Dictionary,
    computeNumericalValue,
    getCipheredWords,
  )

-- | Analyzes a word with the cipher and returns its numerical value
--  and a list of words in the dictionary with the same value
gematria :: Cipher -> Dictionary -> T.Text -> (Int, Maybe [T.Text])
gematria c d w = (v, wordList)
  where
    v = computeNumericalValue c w
    wordList = getCipheredWords c d v

gematriaFormatted :: Cipher -> Dictionary -> T.Text -> a
gematriaFormatted c d w = undefined
  where
    (numValue, wordList) = gematria c d w
