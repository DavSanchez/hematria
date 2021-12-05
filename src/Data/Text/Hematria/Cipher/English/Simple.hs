module Data.Text.Hematria.Cipher.English.Simple (simpleEnglishCipher) where

import Data.Text.Hematria.Cipher.Internal
  ( CipherData,
    buildCipher,
    sameValue,
  )

simpleEnglishCipher :: CipherData
simpleEnglishCipher = buildCipher $ lowerCase <> upperCase
  where
    lowerCase = zip ['a' .. 'z'] [1 ..]
    upperCase = zip ['A' .. 'Z'] [1 ..]