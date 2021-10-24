module CiToolkit.Version.ProgramInput
  ( Command(..)
  , CommonOptions(..)
  , ProgramInput(..)
  , ShowOptions(..)
  , VersionFormat(..)
  , programInputParser
  ) where

import Prelude

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

data ProgramInput = ProgramInput CommonOptions Command

derive instance Generic ProgramInput _

instance Show ProgramInput where
  show = genericShow

data VersionFormat
  = Calendar
  | Semantic

derive instance Generic VersionFormat _

instance Show VersionFormat where
  show = genericShow

data Command
  = Show ShowOptions

derive instance Generic Command _

instance Show Command where
  show = genericShow

newtype ShowOptions = ShowOptions
  { format ∷ VersionFormat }

derive instance Generic ShowOptions _

instance Show ShowOptions where
  show = genericShow

newtype CommonOptions = CommonOptions
  { dryRun ∷ Boolean
  , gitDirectory ∷ FilePath
  , isVerbose ∷ Boolean
  , versionPrefix ∷ VersionTagPrefix
  }

derive instance Generic CommonOptions _

instance Show CommonOptions where
  show = genericShow

programInputParser ∷ Opts.Parser ProgramInput
programInputParser = ado
  opts ← commonOptionsParser
  cmd ← Opts.hsubparser $
    Opts.command "show"
      (Opts.info showCommandParser (Opts.progDesc "show version"))
  in ProgramInput opts cmd

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

commonOptionsParser ∷ Opts.Parser CommonOptions
commonOptionsParser = ado
  dryRun ← Opts.switch (Opts.long "dry-run")
  gitDirectory ← Opts.strOption
    (Opts.long "git-directory" <> Opts.value ".")
  isVerbose ← Opts.switch
    (Opts.long "verbose" <> Opts.short 'v')
  versionPrefix ← Opts.option
    parseVersionPrefix
    ( Opts.long "version-prefix" <>
        (Opts.value $ VersionTagPrefix (nes (Proxy ∷ Proxy "v")))
    )

  in
    CommonOptions
      { dryRun
      , gitDirectory
      , isVerbose
      , versionPrefix
      }

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
