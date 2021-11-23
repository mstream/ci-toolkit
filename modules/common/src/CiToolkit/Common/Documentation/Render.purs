module CiToolkit.Common.Documentation.Render (commandSuiteInfo) where

import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandOption(CommandOption)
  , CommandSuiteInfo(CommandSuiteInfo)
  , HowTo(HowTo)
  , gitDirectoryOption
  )
import CiToolkit.Common.Documentation.Render.Branch as Branch
import CiToolkit.Common.Documentation.Render.Commit as Commit
import CiToolkit.Common.Documentation.Render.Repo as Repo
import Data.Maybe (Maybe(Nothing))

commandSuiteInfo ∷ CommandSuiteInfo
commandSuiteInfo = CommandSuiteInfo
  { commands:
      [ Branch.commandInfo
      , Commit.commandInfo
      , Repo.commandInfo
      ]
  , description:
      [ "The intention of this suite of commands is to keep the track the progress of CI pipelines inside a Git repository instead of CI servers."
      , "In fact, this solution eliminates the necessity of using stateful CI servers at all as each steps of pipelines can be executed by independent, continusously running processes."
      ]
  , howTos: []
  , name: "render"
  }
