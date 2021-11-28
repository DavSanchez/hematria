module Data.Text.Hematria.Cipher where

import Data.Text.Hematria.Cipher.Internal (CipherData)
import Data.Text.Hematria.Cipher.Spanish.Simple
  ( simpleSpanishCipher,
  )

data Cipher
  = SpanishSimple
  | Custom
  deriving (Show)

getCipher :: Cipher -> CipherData
getCipher SpanishSimple = simpleSpanishCipher
getCipher _ = undefined
