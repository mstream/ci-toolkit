module CiToolkit.Common.Documentation.Render.Repo (commandInfo) where

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
  { description: [ "Renders the entire repository." ]
  , howTos:
      [ HowTo
          { codeSnippets:
              [ CodeSnippet
                  { code:
                      [ "npx @ci-toolkit/render repo \\"
                      , "    --format dot | dot -Tpng > /tmp/output.png"
                      ]
                  , title: Nothing
                  }
              ]
          , title: "generate a graphical repository visualization"
          }
      ]
  , name: "repo"
  , options:
      [ ciPrefixOption
      , CommandOption
          { description:
              [ "Format in which the output should be produced." ]
          , longForm: "format"
          , shortForm: Nothing
          }
      , gitDirectoryOption
      ]
  }
