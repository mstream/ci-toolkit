module CiToolkit.Version.ProgramInput
  ( Command(..)
  , CommonOptions(..)
  , ProgramInput(..)
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

data Command
  = TODO

derive instance Generic Command _

instance Show Command where
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
    Opts.command "todo"
      (Opts.info todoCommandParser (Opts.progDesc "TODO"))
  in ProgramInput opts cmd

todoCommandParser ∷ Opts.Parser Command
todoCommandParser = pure TODO

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

parseVersionPrefix ∷ Opts.ReadM VersionTagPrefix
parseVersionPrefix = Opts.eitherReader $ \s →
  case runParser versionTagPrefixParser s of
    Left { error } → Left error
    Right versionPrefix → pure versionPrefix
