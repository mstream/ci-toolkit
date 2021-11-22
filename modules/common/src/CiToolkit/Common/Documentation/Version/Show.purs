module CiToolkit.Common.Documentation.Version.Show (commandInfo) where

import CiToolkit.Common.Documentation
  ( CommandInfo(CommandInfo)
  , CommandOption(CommandOption)
  , HowTo(HowTo)
  , gitDirectoryOption
  )
import Data.Maybe (Maybe(Nothing))

commandInfo ∷ CommandInfo
commandInfo = CommandInfo
  { description: "Calculate a version of the current commit."
  , howTos:
      [ HowTo
          { codeSnippet: "git tag $(npx @ci-toolkit/version show)"
          , title: "create a release tag"
          }
      ]
  , name: "show"
  , options:
      [ CommandOption
          { description: "Format of the version"
          , longForm: "format"
          , shortForm: Nothing
          }
      , gitDirectoryOption
      , CommandOption
          { description: "Prefix for version-related Git tags"
          , longForm: "version-prefix"
          , shortForm: Nothing
          }
      ]
  }
