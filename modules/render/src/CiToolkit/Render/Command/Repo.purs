module CiToolkit.Render.Command.Repo
  ( RepoOptions(..)
  , repoCommandDescription
  , repoOptionsParser
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

repoOptionsParser ∷ Opts.Parser RepoOptions
repoOptionsParser = ado
  ciStagePrefix ← ciStagePrefixParser
  ciStages ← Opts.many $ Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "CI stages order")
  outputFormat ← outputFormatParser
  in RepoOptions { ciStagePrefix, ciStages, outputFormat }

repoCommandDescription ∷ String
repoCommandDescription = "Renders the entire repository."
