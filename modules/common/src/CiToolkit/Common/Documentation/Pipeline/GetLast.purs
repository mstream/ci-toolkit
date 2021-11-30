module CiToolkit.Common.Documentation.Pipeline.GetLast
  ( GetLastOptions(..)
  , commandInfo
  , getLastOptionsParser
  ) where

import Prelude

import CiToolkit.Common.CI (CIStage, CIStagePrefix)
import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandInfo(CommandInfo)
  , CommandOption(CommandOption)
  , HowTo(HowTo)
  , ciStagePrefixOption
  , ciStagePrefixOption
  , commandOptionDescription
  , commandOptionToParser
  , gitDirectoryOption
  )
import CiToolkit.Common.Git.Commit (CommitRef)
import CiToolkit.Common.ProgramInput
  ( dryRunParser
  , parseCIStage
  , parseCommitRef
  )
import Control.Plus (empty)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
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
  ciStagePrefix ← commandOptionToParser ciStagePrefixOption
  ciStages ← Opts.many $ commandOptionToParser ciStageOption
  in GetLastOptions { ciStagePrefix, ciStages }

ciStageOption ∷ CommandOption CIStage
ciStageOption = CommandOption
  { defaultValue: empty
  , description:
      [ "Name of the stage(s) that searched commit needs to be marked with."
      ]
  , longForm: "ci-stage"
  , reader: parseCIStage
  , shortForm: empty
  }

commandInfo ∷ CommandInfo
commandInfo = CommandInfo
  { description:
      [ "Get the last commit which passed given CI stage(s)."
      , "This command uses information produced by the `mark-commit` command to retrieve an identifier of the last commit which passed requested ci-stages."
      ]
  , howTos:
      [ HowTo
          { codeSnippets:
              [ CodeSnippet
                  { code:
                      [ "npx @ci-toolkit/pipeline get-last \\"
                      , "    --ci-stage functional-testing \\"
                      , "    --ci-stage integration-testing"
                      ]
                  , title: mempty
                  }
              ]
          , title:
              "get an ID of the last commit with passed all given CI stages"
          }
      ]
  , name: "get-last"
  , options:
      [ commandOptionDescription ciStagePrefixOption
      , commandOptionDescription ciStageOption
      , commandOptionDescription gitDirectoryOption
      ]
  }
