module CiToolkit.Common.Documentation.Project
  ( projectInfo
  ) where

import Prelude

import CiToolkit.Common.AsciiDoc as ADoc
import CiToolkit.Common.Documentation (ProjectInfo(ProjectInfo))
import CiToolkit.Common.Documentation.Version as Version
import Data.Maybe (Maybe(Nothing))
import Data.String (joinWith)

projectInfo ∷ ProjectInfo
projectInfo = ProjectInfo
  { commandSuites: [ Version.commandSuiteInfo ], name: "CI Toolbox" }
