module CiToolkit.Pipeline.ProgramInput
  ( Command(..)
  , GetLastOptions(..)
  , MarkCommitOptions(..)
  , commandParser
  ) where

import Prelude

import CiToolkit.Common.CI (CIStage)
import CiToolkit.Common.Git.Commit (CommitRef)
import CiToolkit.Common.ProgramInput (parseCIStage, parseCommitRef)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Options.Applicative as Opts

data Command
  = GetLast GetLastOptions
  | MarkCommit MarkCommitOptions

derive instance Generic Command _

instance Show Command where
  show = genericShow

newtype GetLastOptions = GetLastOptions
  { ciStages ∷ List CIStage
  }

newtype MarkCommitOptions = MarkCommitOptions
  { ciStage ∷ CIStage
  , commitRef ∷ CommitRef
  }

derive instance Generic MarkCommitOptions _

instance Show MarkCommitOptions where
  show = genericShow

derive instance Generic GetLastOptions _

instance Show GetLastOptions where
  show = genericShow

commandParser ∷ Opts.Parser Command
commandParser = Opts.hsubparser $
  Opts.command "get-last"
    (Opts.info getLastCommandParser (Opts.progDesc "Get last commit"))
    <> Opts.command "mark-commit"
      ( Opts.info markCommitCommandParser
          (Opts.progDesc "Mark commit")
      )

getLastCommandParser ∷ Opts.Parser Command
getLastCommandParser = ado
  opts ← getLastOptionsParser
  in GetLast opts

markCommitCommandParser ∷ Opts.Parser Command
markCommitCommandParser = ado
  opts ← getMarkCommitOptionsParser
  in MarkCommit opts

getLastOptionsParser ∷ Opts.Parser GetLastOptions
getLastOptionsParser = ado
  ciStages ← Opts.many $ Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "passed stage")
  in GetLastOptions { ciStages }

getMarkCommitOptionsParser ∷ Opts.Parser MarkCommitOptions
getMarkCommitOptionsParser = ado
  ciStage ← Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "stage to be marked with")
  commitRef ← Opts.option parseCommitRef
    (Opts.long "commit-ref" <> Opts.help "commit to be marked")
  in MarkCommitOptions { ciStage, commitRef }
