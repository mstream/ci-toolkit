module CiToolkit.Render.ProgramInput
  ( Command(..)
  , RepoOptions(..)
  , commandParser
  ) where

import Prelude

import CiToolkit.Common.CI
  ( CIStage
  , CIStagePrefix(..)
  )
import CiToolkit.Common.ProgramInput
  ( ciStagePrefixParser
  , outputFormatParser
  , parseCIStage
  )
import CiToolkit.Common.ProgramOutput (OutputFormat(JSON))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Node.Path (FilePath)
import Options.Applicative as Opts
import Type.Proxy (Proxy(Proxy))

data Command
  = Branch
  | Commit
  | Repo RepoOptions
  | Version

derive instance Generic Command _

instance Show Command where
  show = genericShow

newtype RepoOptions = RepoOptions
  { ciStagePrefix ∷ CIStagePrefix
  , ciStages ∷ List CIStage
  , outputFormat ∷ OutputFormat
  }

derive instance Generic RepoOptions _

instance Show RepoOptions where
  show = genericShow

commandParser ∷ Opts.Parser Command
commandParser = Opts.hsubparser $
  Opts.command "branch"
    (Opts.info branchCommandParser (Opts.progDesc "Render branch"))
    <> Opts.command "commit"
      (Opts.info commitCommandParser (Opts.progDesc "Render commit"))
    <> Opts.command "repo"
      ( Opts.info repoCommandParser
          (Opts.progDesc "Render repository")
      )
    <> Opts.command "version"
      ( Opts.info versionCommandParser
          (Opts.progDesc "Print the CLI's version")
      )

branchCommandParser ∷ Opts.Parser Command
branchCommandParser = pure Branch

commitCommandParser ∷ Opts.Parser Command
commitCommandParser = pure Commit

repoCommandParser ∷ Opts.Parser Command
repoCommandParser = ado
  opts ← repoOptionsParser
  in Repo opts

repoOptionsParser ∷ Opts.Parser RepoOptions
repoOptionsParser = ado
  ciStagePrefix ← ciStagePrefixParser
  ciStages ← Opts.many $ Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "CI stages order")
  outputFormat ← outputFormatParser
  in RepoOptions { ciStagePrefix, ciStages, outputFormat }

versionCommandParser ∷ Opts.Parser Command
versionCommandParser = pure Version
