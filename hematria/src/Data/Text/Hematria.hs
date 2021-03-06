{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Hematria where

import Config (Config (cipher, dictionary, num_shown), getConfig)
import Data.Bool (bool)
import qualified Data.IntMap.Strict as IM
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Hematria.Cipher (Cipher, computeNumericalValue, getCharValue, listCiphers, printNumericalValue)
import Data.Text.Hematria.Dictionary
  ( Dictionary,
    DictionaryData,
    buildDictionary,
    getCipheredDictionary,
    getCipheredWords,
    listDicts,
  )
import qualified Data.Text.IO as TextIO
import GHC.Natural (Natural)
import Options
-- ( Command (List, Update, Value),
--   Opts (Cmd, optCipher, optDictionary, optShow, word),
--   Resource (..),
--   execParser,
--   optsParser,
-- )
import System.Exit (exitFailure)
import System.IO (stderr)
import System.Random (Random (randomR), RandomGen, getStdGen, randomRIO, uniformR)

-- | Analyzes a word with the cipher and returns its numerical value
--  and a list of words in the dictionary with the same value
gematria :: Cipher -> DictionaryData -> T.Text -> (Int, Maybe (S.Set T.Text))
gematria c d w = (v, wordList)
  where
    v = computeNumericalValue c w
    wordList = getCipheredWords d v

gematriaFormatted :: Cipher -> DictionaryData -> T.Text -> a
gematriaFormatted c d w = undefined
  where
    (numValue, wordList) = gematria c d w

performGematria :: Options -> IO ()
performGematria opts = do
  defaults <- getConfig
  let c = fromMaybe (cipher defaults) (optCipher opts)
      d = fromMaybe (dictionary defaults) (optDictionary opts)
      n = fromMaybe (num_shown defaults) (optShow opts)
      w = word opts
  dd <- getCipheredDictionary c d
  let (numValue, wordList) = gematria c dd w
  TextIO.putStrLn $ "The numerical value of the word \"" <> w <> "\" is " <> (T.pack . show) numValue <> "."
  case wordList of
    Nothing -> TextIO.putStrLn "No words in the dictionary have the same numerical value."
    Just ws -> do
      words <- randomWordList n ws
      printListed words

randomWordList :: Natural -> S.Set T.Text -> IO [T.Text]
randomWordList 0 ws = pure $ S.toList ws
randomWordList m ws = do
  let siz = fromIntegral (S.size ws) :: Natural
   in if m >= siz
        then randomWordList 0 ws
        else take (fromIntegral m) <$> shuffleWords (S.toList ws)

printListed :: [T.Text] -> IO ()
printListed wl = do
  TextIO.putStrLn "Words in the dictionary with the same numerical value are:\n"
  mapM_ (\d -> TextIO.putStrLn $ "\t- " <> d) wl

-- https://www.programming-idioms.org/idiom/10/shuffle-a-list/826/haskell
shuffleWords :: [T.Text] -> IO [T.Text]
shuffleWords x =
  if length x < 2
    then pure x
    else do
      i <- randomRIO (0, length x - 1)
      r <- shuffleWords (take i x ++ drop (i + 1) x)
      pure (x !! i : r)