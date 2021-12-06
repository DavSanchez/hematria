{-# LANGUAGE LambdaCase #-}

module OptionsParser
  ( execParser,
    optsParser,
    Opts(..),
    Command(..)
  )
where

import Data.Text
-- import Data.Text.Hematria
import Data.Text.Hematria.Cipher
import Data.Text.Hematria.Dictionary
import Options.Applicative

data Opts
  = Cmd Command
  | Gematria
      { -- optUpdateCache :: !Bool,
        optDictionary :: !(Maybe Dictionary),
        optCipher :: !(Maybe Cipher),
        optShow :: !(Maybe Int),
        word :: !Text
      }
  deriving (Show)

data Command = Update deriving (Show)

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    ( fullDesc <> progDesc "optparse subcommands example"
        <> header
          "optparse-sub-example - a small example program for optparse-applicative with subcommands"
    )

versionOption :: Parser (a -> a)
versionOption = infoOption "0.1" (short 'v' <> long "version" <> help "Show version")

programOptions :: Parser Opts
programOptions = cmdOpts <|> gematriaOpts

gematriaOpts :: Parser Opts
gematriaOpts =
  Gematria
    <$> optional
      ( option
          parseDictionary
          ( long "dict"
              <> short 'd'
              <> metavar "DICTIONARY"
              -- <> value "out.txt"
              <> help "Specify dictionary to use"
          )
      )
    <*> optional
      ( option
          parseCipher
          ( long "cipher"
              <> short 'c'
              <> metavar "CIPHER"
              -- <> value "out.txt"
              <> help "Specify cipher to use"
          )
      )
    <*> optional
      ( option
          auto
          ( long "show"
              <> short 's'
              <> metavar "NUMBER"
              <> help "Specify how many (random) words to show."
          )
      )
    <*> strArgument
      ( metavar "WORD"
          <> help "Word with which to perform gematria"
      )

cmdOpts :: Parser Opts
cmdOpts = Cmd <$> subparser update

update :: Mod CommandFields Command
update = command "update" (info (pure Update) (progDesc "Update cache (needed first to work)"))

parseDictionary :: ReadM Dictionary
parseDictionary = eitherReader $ \case
  "spanish" -> pure Spanish
  "english" -> pure English
  _ -> Left "Invalid dictionary"

parseCipher :: ReadM Cipher
parseCipher = eitherReader $ \case
  "simple-es" -> pure SpanishSimple
  "simple-en" -> pure EnglishSimple
  _ -> Left "Invalid cipher"