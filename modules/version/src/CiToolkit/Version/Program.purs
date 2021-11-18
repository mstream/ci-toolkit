module CiToolkit.Version.Program (execute) where

import Prelude

import CiToolkit.Common.CI (loadRepo)
import CiToolkit.Common.Git (getHeadRef)
import CiToolkit.Common.ProgramInput
  ( CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
  )
import CiToolkit.Common.ProgramOutput (ProgramOutput(TextOutput))
import CiToolkit.Version.Calendar (showCalendarVersion)
import CiToolkit.Version.ProgramInput
  ( Command(Show)
  , ShowOptions(ShowOptions)
  , VersionFormat(Calendar, Semantic)
  )
import CiToolkit.Version.Semantic (showSemanticVersion)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Node.Path (FilePath)

execute ∷ ProgramInput Command → Aff ProgramOutput
execute (ProgramInput (CommonOptions commonOpts) command) = do
  repo ← loadRepo commonOpts.gitDirectory commonOpts.ciPrefix
  headRef ← getHeadRef commonOpts.gitDirectory
  let
    result = case command of
      Show (ShowOptions { format }) →
        case format of
          Calendar → showCalendarVersion repo headRef
          Semantic → showSemanticVersion repo headRef

  either
    (throwError <<< error)
    (pure <<< TextOutput)
    result
