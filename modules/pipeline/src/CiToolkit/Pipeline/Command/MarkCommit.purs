module CiToolkit.Pipeline.Command.MarkCommit
  ( MarkCommitOptions(..)
  , markCommitCommandDescription
  , markCommitOptionsParser
  ) where

import Prelude

import CiToolkit.Common.CI (CIStage, CIStagePrefix)
import CiToolkit.Common.Git.Commit (CommitRef)
import CiToolkit.Common.ProgramInput
  ( ciStagePrefixParser
  , dryRunParser
  , parseCIStage
  , parseCommitRef
  )
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Options.Applicative as Opts

data Command
  = GetLast GetLastOptions
  | MarkCommit MarkCommitOptions
  | Version

derive instance Generic Command _

instance Show Command where
  show = genericShow

newtype GetLastOptions = GetLastOptions
  { ciStagePrefix ∷ CIStagePrefix
  , ciStages ∷ List CIStage
  }

newtype MarkCommitOptions = MarkCommitOptions
  { ciStage ∷ CIStage
  , ciStagePrefix ∷ CIStagePrefix
  , commitRef ∷ CommitRef
  , dryRun ∷ Boolean
  }

derive instance Generic MarkCommitOptions _

instance Show MarkCommitOptions where
  show = genericShow

derive instance Generic GetLastOptions _

instance Show GetLastOptions where
  show = genericShow

markCommitOptionsParser ∷ Opts.Parser MarkCommitOptions
markCommitOptionsParser = ado
  ciStage ← Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "stage to be marked with")
  ciStagePrefix ← ciStagePrefixParser
  commitRef ← Opts.option parseCommitRef
    (Opts.long "commit-ref" <> Opts.help "commit to be marked")
  dryRun ← dryRunParser
  in MarkCommitOptions { ciStage, ciStagePrefix, commitRef, dryRun }

markCommitCommandDescription ∷ String
markCommitCommandDescription =
  "Mark a commit as passed a given CI stage."
