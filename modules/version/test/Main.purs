module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.CiToolkit.Version.Calendar as Calendar
import Test.CiToolkit.Version.Program as Program
import Test.CiToolkit.Version.Semantic as Semantic
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = do
  void $ launchAff_ $ runSpec [ consoleReporter ] do
    Calendar.spec
    Program.spec
    Semantic.spec
