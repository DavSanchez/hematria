{-# LANGUAGE OverloadedStrings #-}

module Cache where

-- import qualified Data.ByteString.Char8 as B8

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
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
  putStrLn "Successfully updated cache."

cacheAvailable :: IO Bool
cacheAvailable = doesDirectoryExist =<< getXdgDirectory XdgCache "hematria"

getDictFromCache :: String -> IO [T.Text]
getDictFromCache dict = do
  exists <- doesFileExist =<< getXdgDirectory XdgCache ("hematria/dicts/" <> dict <> ".txt")
  if exists
    then T.lines <$> (TextIO.readFile =<< getXdgDirectory XdgCache ("hematria/dicts/" <> dict <> ".txt"))
    else hPutStrLn stderr ("Dictionary " <> dict <> " not found in cache") >> exitFailure

listCachedDicts :: IO [T.Text]
listCachedDicts = do
  exists <- cacheAvailable
  if exists
    then (getXdgDirectory XdgCache "hematria/dicts" >>= listDirectory) <&> map (stripExtension "txt")
    else hPutStrLn stderr "Cache not found. Run \"hematria update\" to populate." >> exitFailure

stripExtension :: T.Text -> FilePath -> T.Text
stripExtension ext t = fromMaybe packed $ T.stripSuffix ("." <> ext) packed
  where
    packed = T.pack t