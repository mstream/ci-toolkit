module CiToolkit.Common.Documentation.Render.Branch
  ( BranchOptions(..)
  , branchOptionsParser
  , commandInfo
  ) where

import Prelude

import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandInfo(CommandInfo)
  , CommandOption(CommandOption)
  , HowTo(HowTo)
  , gitDirectoryOption
  )
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Show.Generic (genericShow)
import Options.Applicative as Opts

newtype BranchOptions = BranchOptions {}

derive instance Generic BranchOptions _

instance Show BranchOptions where
  show = genericShow

branchOptionsParser ∷ Opts.Parser BranchOptions
branchOptionsParser = pure $ BranchOptions {}

commandInfo ∷ CommandInfo
commandInfo = CommandInfo
  { description: [ "TBA" ]
  , howTos: []
  , name: "branch"
  , options: []
  }
