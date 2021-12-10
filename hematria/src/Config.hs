{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Hematria.Cipher (Cipher (SpanishSimple))
import Data.Text.Hematria.Dictionary (Dictionary (Spanish))
import qualified Data.Text.IO as TextIO
import Data.Yaml (FromJSON, ParseException, decodeFileEither)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import System.Directory (XdgDirectory (XdgConfig), doesFileExist, getXdgDirectory)
import System.IO (stderr)
import Data.Functor (($>))

data Config = Config
  { dictionary :: Dictionary,
    cipher :: Cipher,
    num_shown :: Natural
  }
  deriving (Eq, Show, FromJSON, Generic)

getConfig :: IO Config
getConfig = do
  configFile <- getXdgDirectory XdgConfig "hematria/hematria.yaml"
  exists <- doesFileExist configFile
  if exists
    then do
      conf <- decodeFileEither configFile :: IO (Either ParseException Config)
      either (\_ -> TextIO.hPutStrLn stderr "Could not parse config file. Using defaults." $> defaultConfigEmbedded) pure conf
    else pure defaultConfigEmbedded

defaultConfigEmbedded :: Config
defaultConfigEmbedded =
  Config
    { dictionary = Spanish,
      cipher = SpanishSimple,
      num_shown = 6
    }
