module Main where

import Prelude

import CiToolkit.Common.CLI (run)
import CiToolkit.Pipeline.Program (execute)
import CiToolkit.Pipeline.ProgramInput (programInputParser)
import Effect (Effect)
import Effect.Aff (launchAff_)

main ∷ Effect Unit
main = launchAff_ $ run programInputParser execute
