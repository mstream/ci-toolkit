module CiToolkit.Version.Program (execute) where

import Prelude

import CiToolkit.Common.CI (loadRepo)
import CiToolkit.Common.Documentation.Version.Show
  ( ShowOptions(ShowOptions)
  , VersionFormat(Calendar, Semantic)
  )
import CiToolkit.Common.Git (getHeadRef)
import CiToolkit.Common.ProgramInput
  ( CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
  )
import CiToolkit.Common.ProgramOutput (ProgramOutput(TextOutput))
import CiToolkit.Version.Calendar (showCalendarVersion)
import CiToolkit.Version.Command (Command(Show, Version))
import CiToolkit.Version.Semantic (showSemanticVersion)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Data.Maybe (Maybe(Nothing))
import Effect.Aff (Aff)
import Effect.Exception (error)

execute ∷ String → ProgramInput Command → Aff ProgramOutput
execute version (ProgramInput (CommonOptions commonOpts) command) =
  case command of
    Show (ShowOptions { format }) → do
      repo ← loadRepo commonOpts.gitDirectory Nothing
      headRef ← getHeadRef commonOpts.gitDirectory

      let
        result = case format of
          Calendar → showCalendarVersion repo headRef
          Semantic → showSemanticVersion repo headRef

      either
        (throwError <<< error)
        (pure <<< TextOutput)
        result
    Version → pure $ TextOutput version
