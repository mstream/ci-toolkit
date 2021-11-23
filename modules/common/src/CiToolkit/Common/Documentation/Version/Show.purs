module CiToolkit.Common.Documentation.Version.Show (commandInfo) where

import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandInfo(CommandInfo)
  , CommandOption(CommandOption)
  , HowTo(HowTo)
  , gitDirectoryOption
  )
import Data.Maybe (Maybe(Nothing))

commandInfo ∷ CommandInfo
commandInfo = CommandInfo
  { description: [ "Calculate a version of the current commit." ]
  , howTos:
      [ HowTo
          { codeSnippets:
              [ CodeSnippet
                  { code: [ "git tag $(npx @ci-toolkit/version show)" ]
                  , title: Nothing
                  }
              ]
          , title: "create a release tag"
          }
      ]
  , name: "show"
  , options:
      [ CommandOption
          { description: [ "Format of the version" ]
          , longForm: "format"
          , shortForm: Nothing
          }
      , gitDirectoryOption
      , CommandOption
          { description: [ "Prefix for version-related Git tags" ]
          , longForm: "version-prefix"
          , shortForm: Nothing
          }
      ]
  }
