{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Text.Hematria.Cipher where

import Data.Map.Strict (findWithDefault)
import Data.Text.Hematria.Cipher.English.Simple (simpleEnglishCipher)
import Data.Text.Hematria.Cipher.Internal (CipherData, cipher)
import Data.Text.Hematria.Cipher.Spanish.Simple (simpleSpanishCipher)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

data Cipher
  = SpanishSimple
  | EnglishSimple
  | Custom
  deriving (Show, FromJSON, Generic)

-- | Gets the cipher from the selected from the available ones
--  >>> getCipher SpanishSimple
getCipher :: Cipher -> CipherData
getCipher SpanishSimple = simpleSpanishCipher
getCipher EnglishSimple = simpleEnglishCipher
getCipher _ = undefined

getCharValue :: Cipher -> Char -> Int
getCharValue c ch = findWithDefault 0 ch ((cipher . getCipher) c)