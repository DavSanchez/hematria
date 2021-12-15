module Data.Text.Hematria.Cipher.Spanish.Simple (simpleSpanishCipher) where

import Data.Text.Hematria.Cipher.Internal
  ( CipherData,
    buildCipher,
    sameValue,
  )

simpleSpanishCipher :: CipherData
simpleSpanishCipher = allCases
  where
    lowerCase = zip (['a' .. 'n'] <> ['ñ'] <> ['o' .. 'z']) [1 ..]
    upperCase = zip (['A' .. 'N'] <> ['Ñ'] <> ['O' .. 'Z']) [1 ..]
    basicAlphabet = buildCipher $ lowerCase <> upperCase
    same = sameValue basicAlphabet
    accentedLower =
      same 'á' 'a'
        <> same 'é' 'e'
        <> same 'í' 'i'
        <> same 'ó' 'o'
        <> same 'ú' 'u'
        <> same 'ü' 'u'
    accentedUpper =
      same 'Á' 'A'
        <> same 'É' 'E'
        <> same 'Í' 'I'
        <> same 'Ó' 'O'
        <> same 'Ú' 'U'
        <> same 'Ü' 'U'
    allCases = basicAlphabet <> accentedLower <> accentedUpper