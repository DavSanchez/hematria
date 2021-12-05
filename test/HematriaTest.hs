{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Set as S
import Data.Text.Hematria
import Data.Text.Hematria.Cipher
import Data.Text.Hematria.Dictionary
import Test.Hspec

main :: IO ()
main = hspec simpleTest

simpleTest :: Spec
simpleTest = describe "Gematria tests" $ do
  it "Words should get the correct value for cipher" $ do
    wordWithValue SpanishSimple "Ritual" `shouldBe` (84, S.fromList ["Ritual"])
