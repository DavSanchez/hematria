{-# LANGUAGE OverloadedStrings #-}

module Cache where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import System.Directory
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.URI

dictionaryRepo :: Request
dictionaryRepo = "https://davsanchez.github.io/hematria/data/dicts.tar.gz"

updateCache = do
  createDirectoryIfMissing True =<< getXdgDirectory XdgCache "hematria/dicts"
  let request = dictionaryRepo
      fileName = Prelude.last . pathSegments . getUri $ request
  resp <- httpBS request
  putStrLn $ "Filename: " <> fileName
  B8.putStrLn $ getResponseBody resp

cacheAvailable :: IO Bool
cacheAvailable = doesDirectoryExist =<< getXdgDirectory XdgCache "hematria"

getDefaults :: IO a
getDefaults = undefined

getDictFromCache :: FilePath -> IO [Text]
getDictFromCache dict = do
  exists <- doesFileExist =<< getXdgDirectory XdgCache ("hematria/dicts/" <> dict <> ".txt")
  if exists
    then T.lines <$> (TextIO.readFile =<< getXdgDirectory XdgCache ("hematria/dicts/" <> dict <> ".txt"))
    else hPutStrLn stderr ("Dictionary " <> dict <> " not found in cache") >> exitFailure

listDicts :: IO [FilePath]
listDicts = do
  exists <- cacheAvailable
  if exists
    then listDirectory =<< getXdgDirectory XdgCache "hematria/dicts"
    else hPutStrLn stderr "Cache not found" >> exitFailure