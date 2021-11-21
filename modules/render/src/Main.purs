module Main where

import Prelude

import CiToolkit.Common.CLI (run)
import CiToolkit.Common.ProgramInput (programInputParser)
import CiToolkit.Render.Command (commandParser)
import CiToolkit.Render.Program (execute)
import Effect (Effect)
import Effect.Aff (launchAff_)

main ∷ Effect Unit
main = launchAff_ $ run
  (programInputParser commandParser)
  execute
