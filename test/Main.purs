module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Aff (Aff, catchError, launchAff_, throwError)
import Node.FS.Sync (rmdir)
import Node.OS (tmpdir)
import Node.Path (FilePath)
import Shell (executeCommand)
import Test.CI as CI
import Test.Git as Git
import Test.Git.Commit as GitCommit
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = do
  void $ launchAff_ $ runSpec [ consoleReporter ] do
    CI.spec
    GitCommit.spec
    Git.spec
