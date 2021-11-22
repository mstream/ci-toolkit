module CiToolkit.Common.Documentation.Version (commandSuiteInfo) where

import CiToolkit.Common.Documentation
  ( CommandOption(CommandOption)
  , CommandSuiteInfo(CommandSuiteInfo)
  , HowTo(HowTo)
  , gitDirectoryOption
  )
import CiToolkit.Common.Documentation.Version.Show as VersionShow
import Data.Maybe (Maybe(Nothing))

commandSuiteInfo ∷ CommandSuiteInfo
commandSuiteInfo = CommandSuiteInfo
  { commands: [ VersionShow.commandInfo ]
  , description:
      "The purpose of this suite of commands is to derive the version of a software based on the Git commits history so it is meaningful."
  , name: "version"
  }
