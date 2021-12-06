module Data.Text.Hematria where

import Cache (cacheAvailable, updateCache)
import Config (Config (cipher, dictionary, num_shown), getConfig)
import Data.Bool (bool)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Hematria.Cipher (Cipher, getCharValue)
import Data.Text.Hematria.Dictionary
  ( Dictionary,
    DictionaryData,
    buildDictionary,
    computeNumericalValue,
    getCipheredDictionary,
    getCipheredWords,
  )
import OptionsParser
  ( Command (Update),
    Opts (Cmd, optCipher, optDictionary, optShow, word),
    execParser,
    optsParser,
  )

-- | Analyzes a word with the cipher and returns its numerical value
--  and a list of words in the dictionary with the same value
gematria :: Cipher -> DictionaryData -> T.Text -> (Int, Maybe [T.Text])
gematria c d w = (v, wordList)
  where
    v = computeNumericalValue c w
    wordList = getCipheredWords d v

gematriaFormatted :: Cipher -> DictionaryData -> T.Text -> a
gematriaFormatted c d w = undefined
  where
    (numValue, wordList) = gematria c d w

parseOptions :: IO Opts
parseOptions = execParser optsParser

applyCommand :: Opts -> IO ()
applyCommand (Cmd Update) = updateCache
applyCommand opts = cacheAvailable >>= bool (error "No cache found") (performGematria opts)

performGematria :: Opts -> IO ()
performGematria opts = do
  defaults <- getConfig
  let c = fromMaybe (cipher defaults) (optCipher opts)
      d = fromMaybe (dictionary defaults) (optDictionary opts)
      n = fromMaybe (num_shown defaults) (optShow opts) -- Select at random from List
      w = word opts
  dd <- getCipheredDictionary c d
  let (numValue, wordList) = gematria c dd w
  -- Perform random choosing, whith 0 selecting all words
  putStrLn $ "The numerical value of " <> T.unpack w <> " is " <> show numValue
  case wordList of
    Nothing -> putStrLn "No words in the dictionary have the same numerical value"
    Just ws -> putStrLn $ "The words in the dictionary with the same numerical value are: " <> show ws
