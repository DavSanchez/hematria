module MyLib (someFunc) where

import qualified Data.Map.Strict as M

data Cipher
  = Simple CipherData
  | Custom CipherData

newtype CipherData = CipherData (M.Map Char Int)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getCipher :: Cipher -> CipherData
getCipher (Simple c) = simpleCipher
getCipher _ = undefined

simpleCipher :: CipherData
simpleCipher = CipherData $ M.fromList $ zip ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'ñ', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'] [1 ..]
