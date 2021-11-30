module CiToolkit.Common.Documentation.Pipeline.MarkCommit
  ( MarkCommitOptions(..)
  , commandInfo
  , markCommitOptionsParser
  ) where

import Prelude

import CiToolkit.Common.CI (CIStage, CIStagePrefix)
import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandInfo(CommandInfo)
  , CommandOption(CommandOption)
  , HowTo(HowTo)
  , ciStagePrefixOption
  , commandOptionDescription
  , commandOptionToParser
  , dryRunOption
  , gitDirectoryOption
  , verboseOption
  )
import CiToolkit.Common.Git.Commit (CommitRef)
import CiToolkit.Common.ProgramInput
  ( ciStagePrefixParser
  , dryRunParser
  , parseCIStage
  , parseCommitRef
  )
import Control.Plus (empty)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Options.Applicative as Opts

newtype MarkCommitOptions = MarkCommitOptions
  { ciStage ∷ CIStage
  , ciStagePrefix ∷ CIStagePrefix
  , commitRef ∷ CommitRef
  , dryRun ∷ Boolean
  }

derive instance Generic MarkCommitOptions _

instance Show MarkCommitOptions where
  show = genericShow

markCommitOptionsParser ∷ Opts.Parser MarkCommitOptions
markCommitOptionsParser = ado
  ciStage ← commandOptionToParser ciStageOption
  ciStagePrefix ← commandOptionToParser ciStagePrefixOption
  commitRef ← commandOptionToParser commitRefOption
  dryRun ← commandOptionToParser dryRunOption
  in MarkCommitOptions { ciStage, ciStagePrefix, commitRef, dryRun }

commandInfo ∷ CommandInfo
commandInfo = CommandInfo
  { description:
      [ "Mark a commit as passed a given CI stage."
      , "That information is preserved in a Git repository in the form of commit notes and used by the `get-last` command to query for the last commit which passed certain stages."
      ]
  , howTos:
      [ HowTo
          { codeSnippets:
              [ CodeSnippet
                  { code:
                      [ "npx @ci-toolkit/pipeline mark-commit \\"
                      , "    --ci-stage functional-testing \\"
                      , "    --commit-ref $(git rev-parse HEAD)"
                      ]
                  , title: mempty
                  }
              ]
          , title: "mark the current commit"
          }
      ]
  , name: "mark-commit"
  , options:
      [ commandOptionDescription ciStagePrefixOption
      , commandOptionDescription ciStageOption
      , commandOptionDescription commitRefOption
      , commandOptionDescription dryRunOption
      , commandOptionDescription gitDirectoryOption
      , commandOptionDescription verboseOption
      ]
  }

ciStageOption ∷ CommandOption CIStage
ciStageOption = CommandOption
  { defaultValue: empty
  , description: [ "Name of the stage for commit to be marked with." ]
  , longForm: "ci-stage"
  , reader: parseCIStage
  , shortForm: empty
  }

commitRefOption ∷ CommandOption CommitRef
commitRefOption = CommandOption
  { defaultValue: empty
  , description:
      [ "ID of the commit to be marked with CI stage name." ]
  , longForm: "commit-ref"
  , reader: parseCommitRef
  , shortForm: empty
  }
