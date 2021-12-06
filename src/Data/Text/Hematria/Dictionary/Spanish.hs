{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Text.Hematria.Dictionary.Spanish where

import qualified Data.ByteString as B
import Data.FileEmbed ( embedFile )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )

spanishWordList :: [T.Text]
spanishWordList = T.lines $ decodeUtf8 $(embedFile "docs/data/es")
