module CiToolkit.Common.Documentation.Version (commandSuiteInfo) where

import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandOption(CommandOption)
  , CommandSuiteInfo(CommandSuiteInfo)
  , HowTo(HowTo)
  , gitDirectoryOption
  )
import CiToolkit.Common.Documentation.Version.Show as Show
import Data.Maybe (Maybe(Nothing))

commandSuiteInfo ∷ CommandSuiteInfo
commandSuiteInfo = CommandSuiteInfo
  { commands: [ Show.commandInfo ]
  , description:
      [ "The purpose of this suite of commands is "
      , "to derive the version of a software "
      , "based on the Git commits history so it is meaningful."
      ]
  , howTos:
      [ HowTo
          { codeSnippets:
              [ CodeSnippet { code: [ "" ], title: Nothing } ]
          , title: "create a release tag"
          }
      ]
  , name: "version"
  }
