{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Text.Hematria.Dictionary.Spanish where

import qualified Data.ByteString as B
import Data.FileEmbed
import qualified Data.Text as T
import Data.Text.Encoding

wordList :: [T.Text]
wordList = T.lines $ decodeUtf8 $(embedFile "data/0_palabras_todas.txt")
