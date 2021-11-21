module CiToolkit.Common.ProgramInput
  ( CommonOptions(..)
  , ProgramInput(..)
  , ciStagePrefixParser
  , commonOptionsParser
  , dryRunParser
  , outputFormatParser
  , parseCIPrefix
  , parseCIStage
  , parseCommitRef
  , parseOutputFormat
  , programInputParser
  ) where

import Prelude

import CiToolkit.Common.CI
  ( CIStage
  , CIStagePrefix(..)
  , ciPrefixParser
  , ciStageParser
  )
import CiToolkit.Common.Git.Commit (CommitRef, commitRefParser)
import CiToolkit.Common.ProgramOutput (OutputFormat(DOT, JSON, Text))
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Node.Path (FilePath)
import Options.Applicative as Opts
import Text.Parsing.StringParser (runParser)
import Type.Proxy (Proxy(Proxy))

data ProgramInput c = ProgramInput CommonOptions c

derive instance Generic (ProgramInput c) _

instance (Show c) ⇒ Show (ProgramInput c) where
  show = genericShow

newtype CommonOptions = CommonOptions
  { gitDirectory ∷ FilePath
  , verbose ∷ Boolean
  }

derive instance Generic CommonOptions _

instance Show CommonOptions where
  show = genericShow

programInputParser ∷ ∀ c. Opts.Parser c → Opts.Parser (ProgramInput c)
programInputParser commandParser = ado
  opts ← commonOptionsParser
  cmd ← commandParser
  in ProgramInput opts cmd

commonOptionsParser ∷ Opts.Parser CommonOptions
commonOptionsParser = ado
  gitDirectory ← gitDirectoryParser
  verbose ← verboseParser
  in
    CommonOptions { gitDirectory, verbose }

ciStagePrefixParser ∷ Opts.Parser CIStagePrefix
ciStagePrefixParser = Opts.option
  parseCIPrefix
  ( Opts.long "ci-prefix" <>
      (Opts.value $ CIStagePrefix (nes (Proxy ∷ Proxy "ci-")))
  )

ciStagesParser ∷ Opts.Parser (List CIStage)
ciStagesParser = Opts.many $
  Opts.option
    parseCIStage
    (Opts.long "ci-stage" <> Opts.help "passed stage")

dryRunParser ∷ Opts.Parser Boolean
dryRunParser = Opts.switch $
  Opts.long "dry-run"

outputFormatParser ∷ Opts.Parser OutputFormat
outputFormatParser = Opts.option
  parseOutputFormat
  (Opts.long "format" <> Opts.value JSON)

gitDirectoryParser ∷ Opts.Parser FilePath
gitDirectoryParser = Opts.strOption $
  Opts.long "git-directory" <> Opts.value "."

verboseParser ∷ Opts.Parser Boolean
verboseParser = Opts.switch $
  Opts.long "verbose" <> Opts.short 'v'

parseCIPrefix ∷ Opts.ReadM CIStagePrefix
parseCIPrefix = Opts.eitherReader $ \s →
  case runParser ciPrefixParser s of
    Left { error } → Left error
    Right ciPrefix → pure ciPrefix

parseCIStage ∷ Opts.ReadM CIStage
parseCIStage = Opts.eitherReader $ \s →
  case runParser ciStageParser s of
    Left { error } → Left error
    Right ciStage → pure ciStage

parseCommitRef ∷ Opts.ReadM CommitRef
parseCommitRef = Opts.eitherReader $ \s →
  case runParser commitRefParser s of
    Left { error } → Left error
    Right commitRef → pure commitRef

parseOutputFormat ∷ Opts.ReadM OutputFormat
parseOutputFormat = Opts.eitherReader $ \s →
  case s of
    "dot" → pure DOT
    "json" → pure JSON
    "text" → pure Text
    _ → Left "unsupported format"
