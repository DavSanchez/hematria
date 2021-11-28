module Data.Text.Hematria.Cipher.Simple where

import Data.Text.Hematria.Cipher.Types
import qualified Data.Map.Strict as M

simpleCipher :: CipherData
simpleCipher = CipherData $ M.fromList $ zip ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'ñ', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'] [1 ..]