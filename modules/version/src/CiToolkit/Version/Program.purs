module CiToolkit.Version.Program (execute) where

import Prelude

import CiToolkit.Common.ProgramOutput (ProgramOutput(TextOutput))
import CiToolkit.Version.ProgramInput
  ( Command(Show)
  , CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
  , ShowOptions(ShowOptions)
  , VersionFormat(Calendar, Semantic)
  )
import Data.Argonaut (encodeJson)
import Data.DotLang (toGraph)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Node.Path (FilePath)

execute ∷ ProgramInput → Aff ProgramOutput
execute (ProgramInput (CommonOptions commonOpts) command) = do
  case command of
    Show (ShowOptions { format }) →
      case format of
        Calendar → pure $ TextOutput $ "TODO"
        Semantic → pure $ TextOutput $ "TODO"
