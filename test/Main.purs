module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.CI as CI
import Test.Git as Git
import Test.Git.Commit as GitCommit
import Test.Program as Program
import Test.Query as Query
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Update as Update

main ∷ Effect Unit
main = do
  void $ launchAff_ $ runSpec [ consoleReporter ] do
    CI.spec
    GitCommit.spec
    Git.spec
    Program.spec
    Query.spec
    Update.spec
