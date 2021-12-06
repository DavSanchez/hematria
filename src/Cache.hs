{-# LANGUAGE OverloadedStrings #-}

module Cache where

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import System.Directory
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

dictionaryRepo = ""

updateCache = do
  createDirectoryIfMissing True =<< getXdgDirectory XdgCache "hematria/dict"


cacheAvailable :: IO Bool
cacheAvailable = doesDirectoryExist =<< getXdgDirectory XdgCache "hematria"

getDefaults :: IO a
getDefaults = undefined

getDictFromCache :: FilePath -> IO [Text]
getDictFromCache dict = do
  exists <- doesFileExist =<< getXdgDirectory XdgCache ("hematria/dict/" <> dict)
  if exists
    then T.lines <$> (TextIO.readFile =<< getXdgDirectory XdgCache ("hematria/dict/" <> dict))
    else hPutStrLn stderr ("Dictionary " <> dict <> " not found in cache") >> exitFailure

listDicts :: IO [FilePath]
listDicts = do
  exists <- cacheAvailable
  if exists
    then listDirectory =<< getXdgDirectory XdgCache "hematria/dict"
    else hPutStrLn stderr "Cache not found" >> exitFailure