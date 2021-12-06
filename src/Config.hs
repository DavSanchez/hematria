{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Hematria.Cipher (Cipher (SpanishSimple))
import Data.Text.Hematria.Dictionary (Dictionary (Spanish))
import Data.Yaml
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import System.Directory (XdgDirectory (XdgConfig), doesFileExist, getXdgDirectory)
import System.IO (hPutStrLn, stderr)

data Config = Config
  { dictionary :: Dictionary,
    cipher :: Cipher,
    num_shown :: Natural
  }
  deriving (Show, FromJSON, Generic)

getConfig :: IO Config
getConfig = do
  configFile <- getXdgDirectory XdgConfig "hematria/hematria.yaml"
  exists <- doesFileExist configFile
  if exists
    then do
      conf <- decodeFileEither configFile :: IO (Either ParseException Config)
      either (\_ -> hPutStrLn stderr "Could not parse config file. Using defaults." >> pure defaultConfigEmbedded) pure conf
    else pure defaultConfigEmbedded

defaultConfigEmbedded :: Config
defaultConfigEmbedded =
  Config
    { dictionary = Spanish,
      cipher = SpanishSimple,
      num_shown = 10
    }
