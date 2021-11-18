module CiToolkit.Version.ProgramInput
  ( Command(..)
  , ShowOptions(..)
  , VersionFormat(..)
  , commandParser
  ) where

import Prelude

import CiToolkit.Common.ProgramInput (ProgramInput(ProgramInput))
import CiToolkit.Common.Version
  ( VersionTagPrefix(VersionTagPrefix)
  , versionTagPrefixParser
  )
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Node.Path (FilePath)
import Options.Applicative as Opts
import Text.Parsing.StringParser (runParser)
import Type.Proxy (Proxy(Proxy))

data VersionFormat
  = Calendar
  | Semantic

derive instance Generic VersionFormat _

instance Show VersionFormat where
  show = genericShow

data Command = Show ShowOptions

derive instance Generic Command _

instance Show Command where
  show = genericShow

newtype ShowOptions = ShowOptions
  { format ∷ VersionFormat }

derive instance Generic ShowOptions _

instance Show ShowOptions where
  show = genericShow

commandParser ∷ Opts.Parser Command
commandParser =
  Opts.hsubparser $ Opts.command
    "show"
    ( Opts.info
        showCommandParser
        (Opts.progDesc "show version")
    )

showCommandParser ∷ Opts.Parser Command
showCommandParser = ado
  opts ← showOptionsParser
  in Show opts

showOptionsParser ∷ Opts.Parser ShowOptions
showOptionsParser = ado
  format ← Opts.option parseVersionFormat
    (Opts.long "format" <> Opts.value Semantic)
  in
    ShowOptions { format }

parseVersionFormat ∷ Opts.ReadM VersionFormat
parseVersionFormat = Opts.eitherReader $ \s →
  case s of
    "calendar" → pure Calendar
    "semantic" → pure Semantic
    _ → Left "unsupported format"

parseVersionPrefix ∷ Opts.ReadM VersionTagPrefix
parseVersionPrefix = Opts.eitherReader $ \s →
  case runParser versionTagPrefixParser s of
    Left { error } → Left error
    Right versionPrefix → pure versionPrefix
