module Data.Text.Hematria.Cipher.Types where
  
import qualified Data.Map.Strict as M

data Cipher
  = Simple
  | Custom

newtype CipherData = CipherData
  { cipher :: M.Map Char Int
  }
  deriving (Show)