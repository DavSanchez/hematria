{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Hematria.Cipher where

import Data.Map.Strict (findWithDefault)
import qualified Data.Text as T
import Data.Text.Hematria.Cipher.English.Simple (simpleEnglishCipher)
import Data.Text.Hematria.Cipher.Internal (CipherData, cipher)
import Data.Text.Hematria.Cipher.Spanish.Simple (simpleSpanishCipher)
import qualified Data.Text.IO as TextIO
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

data Cipher
  = SpanishSimple
  | EnglishSimple
  deriving (Show, FromJSON, Generic)

-- | Gets the cipher from the selected from the available ones
--  >>> getCipher SpanishSimple
getCipher :: Cipher -> CipherData
getCipher SpanishSimple = simpleSpanishCipher
getCipher EnglishSimple = simpleEnglishCipher

getCharValue :: Cipher -> Char -> Int
getCharValue c ch = findWithDefault 0 ch ((cipher . getCipher) c)

-- | Computes the numerical value of a word using the cipher
--  >>> computeNumericalValue SpanishSimple "Ritual"
--  65
computeNumericalValue :: Cipher -> T.Text -> Int
computeNumericalValue c w = sum $ map (getCharValue c) (T.unpack w)

listCiphers :: IO ()
listCiphers = do
  TextIO.putStrLn "Available ciphers:\n"
  TextIO.putStrLn "\t- simple-es (simple, ascending value cipher)"
  TextIO.putStrLn "\t- simple-en (simple, ascending value cipher)"

printNumericalValue :: Cipher -> T.Text -> IO ()
printNumericalValue c w = do
  TextIO.putStrLn $ "The numerical value of the word " <> w <> " is " <> (T.pack . show) (computeNumericalValue c w) <> "."
