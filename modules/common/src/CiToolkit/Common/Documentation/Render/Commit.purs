module CiToolkit.Common.Documentation.Render.Commit
  ( CommitOptions(..)
  , commitOptionsParser
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

newtype CommitOptions = CommitOptions {}

derive instance Generic CommitOptions _

instance Show CommitOptions where
  show = genericShow

commitOptionsParser ∷ Opts.Parser CommitOptions
commitOptionsParser = pure $ CommitOptions {}

commandInfo ∷ CommandInfo
commandInfo = CommandInfo
  { description: [ "TBA" ]
  , howTos: []
  , name: "commit"
  , options: []
  }
