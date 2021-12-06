{-# LANGUAGE OverloadedStrings #-}

module Cache where

-- import qualified Data.ByteString.Char8 as B8

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import Network.HTTP.Client (Request, getUri)
import Network.HTTP.Simple (getResponseBody, httpLBS)
import Network.URI (pathSegments)
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    listDirectory,
  )
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

dictionaryRepo :: Request
dictionaryRepo = "https://davsanchez.github.io/hematria/data/dicts.tar.gz"

updateCache :: IO ()
updateCache = do
  cacheDir <- getXdgDirectory XdgCache "hematria/dicts"
  createDirectoryIfMissing True cacheDir
  let fileName = last . pathSegments . getUri $ dictionaryRepo
  resp <- httpLBS dictionaryRepo
  -- LB8.writeFile (cacheDir <> fileName) (getResponseBody resp) -- Store compressed file to disk if needed
  Tar.unpack cacheDir . Tar.read . GZip.decompress $ getResponseBody resp

cacheAvailable :: IO Bool
cacheAvailable = doesDirectoryExist =<< getXdgDirectory XdgCache "hematria"

getDefaults :: IO a
getDefaults = undefined

getDictFromCache :: String -> IO [Text]
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