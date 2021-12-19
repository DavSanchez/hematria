{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens.TH
import Data.Default
import Data.Text (Text)
import Data.Text.Hematria
import Data.Text.Hematria.Cipher
import Data.Text.Hematria.Dictionary
import Monomer

-- data HematriaItem = HematriaItem
--   { _hematriaItemDict :: Dictionary,
--     _hematriaItemCipher :: Cipher,
--     _hematriaItemWord :: Text
--   }
--   deriving (Eq, Show)

-- instance Default HematriaItem where
--   def =
--     HematriaItem
--       { _hematriaItemDict = Spanish,
--         _hematriaItemCipher = SpanishSimple,
--         _hematriaItemWord = ""
--       }

data HematriaModel = HematriaModel
  { -- _hematriaItem :: HematriaItem,
    _hematriaDict :: Dictionary,
    _hematriaCipher :: Cipher,
    _hematriaWord :: Text,
    _hematriaResult :: [Text]
  }
  deriving (Eq, Show)

instance Default HematriaModel where
  def =
    HematriaModel
      { _hematriaDict = Spanish,
        _hematriaCipher = SpanishSimple,
        _hematriaWord = "",
        _hematriaResult = []
      }

makeLenses 'HematriaModel

-- makeLenses 'HematriaItem