{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Config (Config (..), getConfig)
import Control.Monad ((>=>))
import Data.Functor ((<&>))
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.Hematria
import Data.Text.Hematria.Cipher
import Data.Text.Hematria.Dictionary
import qualified Data.Text.IO as TextIO
import GHC.Natural (Natural)
import System.Directory
import Test.Hspec

sampleWordList :: [Text]
sampleWordList = ["Brujería  ", "  Viaje Astral", " Hechizos ", "  Conjuros. . ", "Lujuria", "Ritual", " ^ éter ", "Ónice"]

sanitizedSampleWordList :: [Text]
sanitizedSampleWordList = ["Brujería", "Viaje Astral", "Hechizos", "Conjuros. .", "Lujuria", "Ritual", "^ Éter", "Ónice"]

buildSampleDictionary :: IO [Text]
buildSampleDictionary = undefined sampleWordList

main :: IO ()
main = hspec $ beforeAll cleanup spec

spec :: Spec
spec =
  describe "Gematria tests: Simple Spanish Cipher" $
    sanitizeWordListTest
      >> getValidConfig
      >> simpleSpanishTest
      >> simpleEnglishTest

sanitizeWordListTest :: Spec
sanitizeWordListTest =
  it "Should correctly sanitize the dict word list" $
    sanitize sampleWordList `shouldBe` sanitizedSampleWordList

getValidConfig :: Spec
getValidConfig =
  it "Should correctly get the config" $ do
    writeConfig
    conf <- getConfig
    let expectedConf =
          Config
            { dictionary = English,
              cipher = EnglishSimple,
              num_shown = 3 :: Natural
            }
    conf `shouldBe` expectedConf

simpleSpanishTest :: Spec
simpleSpanishTest = it "Words should get the correct value for cipher" $ do
  wordWithValue SpanishSimple "Ritual" `shouldBe` (84, S.fromList ["Ritual"])

simpleEnglishTest :: Spec
simpleEnglishTest = it "Words should get the correct value for cipher" $ do
  wordWithValue EnglishSimple "Ritual" `shouldBe` (81, S.fromList ["Ritual"])

cleanupAppDir :: XdgDirectory -> IO ()
cleanupAppDir xdgDir = do
  dir <- getXdgDirectory xdgDir "hematria"
  exists <- doesPathExist dir
  if exists then removeDirectoryRecursive dir else pure ()

cleanup :: IO ()
cleanup = cleanupAppDir XdgCache >> cleanupAppDir XdgConfig

writeConfig :: IO ()
writeConfig = do
  createDirectoryIfMissing True =<< getXdgDirectory XdgConfig "hematria"
  getXdgDirectory XdgConfig "hematria/hematria.yaml" >>= copyFile "test/hematria-config-sample.yaml"