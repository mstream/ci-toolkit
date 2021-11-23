module CiToolkit.Common.Documentation.Render.Commit (commandInfo) where

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
  { description: [ "TBA" ]
  , howTos: []
  , name: "commit"
  , options: []
  }
