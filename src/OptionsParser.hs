{-# LANGUAGE LambdaCase #-}

module OptionsParser
  ( execParser,
    optsParser,
    Opts (..),
    Command (..),
    Resource (..),
  )
where

import Data.Text (Text)
import Data.Text.Hematria.Cipher (Cipher (..))
import Data.Text.Hematria.Dictionary
  ( Dictionary (English, Spanish),
  )
import GHC.Natural (Natural)
import Options.Applicative
  ( Alternative ((<|>)),
    CommandFields,
    Mod,
    Parser,
    ParserInfo,
    ReadM,
    argument,
    auto,
    command,
    eitherReader,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    infoOption,
    long,
    metavar,
    option,
    optional,
    progDesc,
    short,
    strArgument,
  )

data Opts
  = Cmd Command
  | Gematria
      { -- optUpdateCache :: !Bool,
        optDictionary :: !(Maybe Dictionary),
        optCipher :: !(Maybe Cipher),
        optShow :: !(Maybe Natural),
        word :: !Text
      }
  deriving (Show)

data Command
  = Update
  | List Resource
  | Value (Maybe Cipher) Text
  deriving (Show)

data Resource
  = Cipher
  | Dictionary
  deriving (Show)

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    ( fullDesc <> progDesc "Perform gematria from the command line."
        <> header
          "Hematria - Perform gematria from the command line. Done with Haskell."
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
cmdOpts = Cmd <$> hsubparser (update <> list <> numValue)

update :: Mod CommandFields Command
update = command "update" (info (pure Update) (progDesc "Update cache (needed first to work)"))

list :: Mod CommandFields Command
list = command "list" (info listOpts (progDesc "List available resources (ciphers, dictionaries)"))

listOpts :: Parser Command
listOpts = List <$> argument parseResource (metavar "RESOURCE" <> help "Resource to list")

numValue :: Mod CommandFields Command
numValue = command "value" (info numValueOpts (progDesc "Get the numerical value of a word (cipher-dependent)"))

numValueOpts :: Parser Command
numValueOpts =
  Value
    <$> optional
      ( option
          parseCipher
          ( long "cipher"
              <> short 'c'
              <> metavar "CIPHER"
              -- <> value "out.txt"
              <> help "Specify cipher to use"
          )
      )
      <*> strArgument
        (metavar "WORD")

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

parseResource :: ReadM Resource
parseResource = eitherReader $ \case
  "ciphers" -> pure Cipher
  "cipher" -> pure Cipher
  "cphrs" -> pure Cipher
  "cphr" -> pure Cipher
  "dictionaries" -> pure Dictionary
  "dictionary" -> pure Dictionary
  "dicts" -> pure Dictionary
  "dict" -> pure Dictionary
  _ -> Left "Invalid resource"