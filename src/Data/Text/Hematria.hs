module Data.Text.Hematria where

import Cache (cacheAvailable, updateCache)
import Config (getDefaults)
import Data.Bool (bool)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Hematria.Cipher (Cipher, getCharValue)
import Data.Text.Hematria.Dictionary
  ( Dictionary,
    computeNumericalValue,
    getCipheredWords,
  )
import OptionsParser

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

parseOptions :: IO Opts
parseOptions = execParser optsParser

applyCommand :: Opts -> IO ()
applyCommand (Cmd Update) = updateCache
applyCommand opts = cacheAvailable >>= bool (error "No cache found") (performGematria opts)

performGematria opts = do
  defaults <- getDefaults
  let c = flip fromMaybe (optCipher opts) undefined -- (undefined) Get default Cipher
      d = flip fromMaybe (optDictionary opts) undefined -- (undefined) Get default Dictionary
      w = word opts
      (numValue, wordList) = gematria c d w
  putStrLn $ "The numerical value of " <> T.unpack w <> " is " <> show numValue
  case wordList of
    Nothing -> putStrLn "No words in the dictionary have the same numerical value"
    Just ws -> putStrLn $ "The words in the dictionary with the same numerical value are: " <> show ws
