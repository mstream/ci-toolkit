module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.CI as CI
import Test.Git as Git
import Test.Git.Commit as GitCommit
import Test.Git.Tag as GitTag
import Test.Print as Print
import Test.Program as Program
import Test.Query as Query
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Update as Update

main ∷ Effect Unit
main = do
  void $ launchAff_ $ runSpec [ consoleReporter ] do
    CI.spec
    Git.spec
    GitCommit.spec
    GitTag.spec
    Print.spec
    Program.spec
    Query.spec
    Update.spec
