{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.Hematria
import Data.Text.Hematria.Cipher
import Data.Text.Hematria.Dictionary
import Test.Hspec

sampleWordList :: [Text]
sampleWordList = ["Brujería", "Viaje Astral", "Hechizos", "Conjuros", "Lujuria", "Ritual", "éter", "Ónice"]

buildSampleDictionary :: IO [Text]
buildSampleDictionary = pure sampleWordList

main :: IO ()
main = hspec $ beforeAll cleanup spec

spec :: Spec
spec =
  simpleSpanishTest
    >> simpleEnglishTest

simpleSpanishTest :: Spec
simpleSpanishTest = describe "Gematria tests: Simple Spanish Cipher" $ do
  it "Words should get the correct value for cipher" $ do
    wordWithValue SpanishSimple "Ritual" `shouldBe` (84, S.fromList ["Ritual"])

simpleEnglishTest :: Spec
simpleEnglishTest = describe "Gematria tests: Simple English Cipher" $ do
  it "Words should get the correct value for cipher" $ do
    wordWithValue EnglishSimple "Ritual" `shouldBe` (81, S.fromList ["Ritual"])

cleanupCache :: IO ()
cleanupCache = undefined -- Cleanup Cache

cleanupConfig :: IO ()
cleanupConfig = undefined -- Cleanup Config

cleanup :: IO ()
cleanup = cleanupCache <> cleanupConfig -- How does this work?
