module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = do
  void $ launchAff_ $ runSpec [ consoleReporter ] do
    pure unit
