module CiToolkit.Pipeline.Command.GetLast
  ( GetLastOptions(..)
  , getLastCommandDescription
  , getLastOptionsParser
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

newtype GetLastOptions = GetLastOptions
  { ciStagePrefix ∷ CIStagePrefix
  , ciStages ∷ List CIStage
  }

derive instance Generic GetLastOptions _

instance Show GetLastOptions where
  show = genericShow

getLastOptionsParser ∷ Opts.Parser GetLastOptions
getLastOptionsParser = ado
  ciStagePrefix ← ciStagePrefixParser
  ciStages ← Opts.many $ Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "passed stage")
  in GetLastOptions { ciStagePrefix, ciStages }

getLastCommandDescription ∷ String
getLastCommandDescription =
  "Get the last commit which passed given CI stage(s)."
