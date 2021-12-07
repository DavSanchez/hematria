{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Text.Hematria.Cipher where

import Data.Map.Strict (findWithDefault)
import qualified Data.Text as T
import Data.Text.Hematria.Cipher.English.Simple (simpleEnglishCipher)
import Data.Text.Hematria.Cipher.Internal (CipherData, cipher)
import Data.Text.Hematria.Cipher.Spanish.Simple (simpleSpanishCipher)
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
  putStrLn "Available ciphers:"
  putStrLn "\t- simple-es (simple, ascending value cipher)"
  putStrLn "\t- simple-en (simple, ascending value cipher)"

printNumericalValue :: Cipher -> T.Text -> IO ()
printNumericalValue c w = do
  putStrLn $ "Numerical value of word " <> T.unpack w <> " is " <> show (computeNumericalValue c w) <> "."
