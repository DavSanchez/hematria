module Data.Text.Hematria.Cipher.Internal where

import qualified Data.Map.Strict as M
import Data.Maybe

newtype CipherData = CipherData
  { cipher :: M.Map Char Int
  }
  deriving (Eq, Show)

instance Semigroup CipherData where
  CipherData a <> CipherData b = CipherData $ a <> b

instance Monoid CipherData where
  mempty = CipherData mempty

sameValue :: CipherData -> Char -> Char -> CipherData
sameValue a c1 c2 = a <> CipherData (M.singleton c1 v)
  where
    v = fromMaybe 0 (M.lookup c2 (cipher a))

buildCipher :: [(Char, Int)] -> CipherData
buildCipher = CipherData . M.fromList