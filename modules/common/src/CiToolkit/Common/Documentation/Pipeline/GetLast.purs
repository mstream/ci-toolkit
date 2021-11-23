module CiToolkit.Common.Documentation.Pipeline.GetLast (commandInfo) where

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
                  , title: Nothing
                  }
              ]
          , title:
              "get an ID of the last commit with passed all given CI stages"
          }
      ]
  , name: "get-last"
  , options:
      [ ciPrefixOption
      , CommandOption
          { description:
              [ "Name of the stage(s) that searched commit needs to be marked with."
              ]
          , longForm: "ci-stage"
          , shortForm: Nothing
          }
      , gitDirectoryOption
      ]
  }
