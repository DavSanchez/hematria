{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as S
import Data.Text.Hematria
import Data.Text.Hematria.Cipher
import Data.Text.Hematria.Dictionary
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec 
  = simpleSpanishTest
  >> simpleEnglishTest

simpleSpanishTest :: Spec
simpleSpanishTest = describe "Gematria tests: Simple Spanish Cipher" $ do
  it "Words should get the correct value for cipher" $ do
    wordWithValue SpanishSimple "Ritual" `shouldBe` (84, S.fromList ["Ritual"])

simpleEnglishTest :: Spec
simpleEnglishTest = describe "Gematria tests: Simple English Cipher" $ do
  it "Words should get the correct value for cipher" $ do
    wordWithValue EnglishSimple "Ritual" `shouldBe` (81, S.fromList ["Ritual"])
