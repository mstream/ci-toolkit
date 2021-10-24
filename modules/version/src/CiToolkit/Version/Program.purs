module CiToolkit.Version.Program (execute) where

import Prelude

import CiToolkit.Common.ProgramOutput (ProgramOutput(TextOutput))
import CiToolkit.Version.ProgramInput
  ( Command(TODO)
  , CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
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

    TODO → pure $ TextOutput $ "TODO"
