module Main where

import Prelude

import CiToolkit.Common.CLI (run)
import CiToolkit.Render.Program (execute)
import CiToolkit.Render.ProgramInput (programInputParser)
import Effect (Effect)
import Effect.Aff (launchAff_)

main ∷ Effect Unit
main = launchAff_ $ run programInputParser execute
