module Data.Text.Hematria.Dictionary.Types where
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Text as T

data Dictionary
  = Sample
  | Spanish
  | English

newtype DictionaryData = Dictionary
  { dict :: IM.IntMap (S.Set T.Text)
  }
  deriving (Show)