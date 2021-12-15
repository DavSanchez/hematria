{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens.TH
import Data.Default
import Data.Text (Text)
import Data.Text.Hematria
import Data.Text.Hematria.Cipher
import Data.Text.Hematria.Dictionary
import Monomer

data HematriaItem = HematriaItem
  { _hematriaItemDict :: Dictionary,
    _hematriaItemCipher :: Cipher,
    _hematriaItemWord :: Text
  }
  deriving (Eq, Show)

instance Default HematriaItem where
  def =
    HematriaItem
      { _hematriaItemDict = Spanish,
        _hematriaItemCipher = SpanishSimple,
        _hematriaItemWord = ""
      }

newtype HematriaModel = HematriaModel
  { _activeHematriaItem :: HematriaItem
  }
  deriving (Eq, Show)

makeLenses 'HematriaModel
makeLenses 'HematriaItem