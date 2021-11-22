module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.CiToolkit.Common.CI as CI
import Test.CiToolkit.Common.Documentation as Documentation
import Test.CiToolkit.Common.Git as Git
import Test.CiToolkit.Common.Git.Commit as GitCommit
import Test.CiToolkit.Common.Git.Commit.UserInfo as GitCommitUserInfo
import Test.CiToolkit.Common.Git.Tag as GitTag
import Test.CiToolkit.Common.Print as Print
import Test.CiToolkit.Common.Query as Query
import Test.CiToolkit.Common.Update as Update
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = do
  void $ launchAff_ $ runSpec [ consoleReporter ] do
    CI.spec
    Documentation.spec
    Git.spec
    GitCommit.spec
    GitCommitUserInfo.spec
    GitTag.spec
    Print.spec
    Query.spec
    Update.spec
