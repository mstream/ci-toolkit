module CiToolkit.Common.Documentation.Pipeline.MarkCommit (commandInfo) where

import Prelude

import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandInfo(CommandInfo)
  , CommandOption(CommandOption)
  , HowTo(HowTo)
  , ciPrefixOption
  , gitDirectoryOption
  )
import Data.Maybe (Maybe(Nothing))

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
                  , title: Nothing
                  }
              ]
          , title: "mark the current commit"
          }
      ]
  , name: "mark-commit"
  , options:
      [ ciPrefixOption
      , CommandOption
          { description:
              [ "Name of the stage for commit to be marked with." ]
          , longForm: "ci-stage"
          , shortForm: Nothing
          }
      , CommandOption
          { description:
              [ "ID of the commit to be marked with CI stage name." ]
          , longForm: "commit-ref"
          , shortForm: Nothing
          }
      , CommandOption
          { description:
              [ "When enabled, a simulated outcome of the marking is displayed but no change to the Git repository is made."
              ]
          , longForm: "dry-run"
          , shortForm: Nothing
          }
      , gitDirectoryOption
      , CommandOption
          { description:
              [ "Output the maximum amount of diagnostic information." ]
          , longForm: "verbose"
          , shortForm: pure "v"
          }
      ]
  }
