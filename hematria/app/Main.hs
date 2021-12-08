{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cache (cacheAvailable, updateCache)
import Config (Config (cipher), getConfig)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Text.Hematria (performGematria)
import Data.Text.Hematria.Cipher (listCiphers, printNumericalValue)
import Data.Text.Hematria.Dictionary (listDicts)
import qualified Data.Text.IO as TextIO
import OptionsParser
  ( Command (List, Update, Value),
    Opts (Cmd, Gematria),
    Resource (Cipher, Dictionary),
    execParser,
    optsParser,
  )
import System.Exit (exitFailure)
import System.IO (stderr)

main :: IO ()
main = applyCommand =<< parseOptions

parseOptions :: IO Opts
parseOptions = execParser optsParser

applyCommand :: Opts -> IO ()
applyCommand (Cmd Update) = updateCache
applyCommand (Cmd (List Dictionary)) = listDicts
applyCommand (Cmd (List Cipher)) = listCiphers
applyCommand (Cmd (Value c w)) = getConfig >>= \conf -> printNumericalValue (fromMaybe (cipher conf) c) w
applyCommand (Gematria opts) = cacheAvailable >>= bool (TextIO.hPutStrLn stderr "Cache not found. Run \"hematria update\" to populate." >> exitFailure) (performGematria opts)