module Options where

import Data.Text (Text)
import Data.Text.Hematria.Cipher (Cipher)
import Data.Text.Hematria.Dictionary (Dictionary)
import GHC.Natural (Natural)

data Options = Options
  { -- optUpdateCache :: !Bool,
    optDictionary :: !(Maybe Dictionary),
    optCipher :: !(Maybe Cipher),
    optShow :: !(Maybe Natural),
    word :: !Text
  } deriving (Show)