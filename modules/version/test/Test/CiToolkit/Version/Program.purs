module Test.CiToolkit.Version.Program (spec) where

import Prelude

import CiToolkit.Common.CI (CIStagePrefix(CIStagePrefix))
import CiToolkit.Common.ProgramInput
  ( CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
  )
import CiToolkit.Common.ProgramOutput
  ( OutputFormat(Text)
  , ProgramOutput(TextOutput)
  )
import CiToolkit.Common.Utils
  ( unsafeDate
  , unsafeNonEmptyString
  , unsafeTimeFromHours
  )
import CiToolkit.Version.Program (execute)
import CiToolkit.Version.ProgramInput
  ( Command(Show)
  , ShowOptions(ShowOptions)
  , VersionFormat(Calendar)
  )
import Data.DateTime (DateTime(DateTime))
import Data.List (List(Nil))
import Data.Maybe (Maybe(Nothing))
import Test.CiToolkit.Common.TestUtils (createCommit, withGitRepo)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Program" do
  describe "execute" do
    around withGitRepo do
      it "calculates a valid version" $ \gitDirPath → do
        let
          testCommitInfo parentRef date time =
            { authorName: "user1"
            , committerName: "user2"
            , date: DateTime date time
            , message: "commit message"
            , parentRef
            }

          createExampleCommit parentRef date time =
            createCommit
              gitDirPath
              (testCommitInfo parentRef date time)

        void $ createExampleCommit
          Nothing
          (unsafeDate { year: 2020, month: 5, day: 1 })
          (unsafeTimeFromHours 1)

        void $ createExampleCommit
          Nothing
          (unsafeDate { year: 2020, month: 6, day: 1 })
          (unsafeTimeFromHours 1)

        void $ createExampleCommit
          Nothing
          (unsafeDate { year: 2020, month: 6, day: 10 })
          (unsafeTimeFromHours 1)

        void $ createExampleCommit
          Nothing
          (unsafeDate { year: 2020, month: 6, day: 10 })
          (unsafeTimeFromHours 2)

        let
          expected = TextOutput "2020.06.10_2"

          commonOpts = CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , ciStages: Nil
            , dryRun: false
            , format: Text
            , gitDirectory: gitDirPath
            , isVerbose: false
            }

          command = Show $ ShowOptions { format: Calendar }

        actual ← execute $ ProgramInput commonOpts command

        actual `shouldEqual` expected
