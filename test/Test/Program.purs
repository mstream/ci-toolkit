module Test.Program (spec) where

import Prelude

import CI (CIStage(CIStage), CIStagePrefix(CIStagePrefix), Repo(Repo))
import Data.Argonaut (encodeJson)
import Data.Date (Date)
import Data.Date as Date
import Data.DateTime (DateTime(DateTime))
import Data.Enum (toEnum)
import Data.List (List(Nil), fromFoldable)
import Data.Maybe (fromJust)
import Data.Time (Time)
import Data.Time as Time
import Data.Tuple.Nested ((/\))
import Git.Commit (asHex)
import Partial.Unsafe (unsafePartial)
import Program (execute)
import ProgramInput
  ( ProgramInput(ProgramInput)
  , Command(GetLast, MarkCommit, Render)
  , CommonOptions(CommonOptions)
  , GetLastOptions(GetLastOptions)
  , MarkCommitOptions(MarkCommitOptions)
  , RenderFormat(JSON)
  , RenderOptions(RenderOptions)
  )
import ProgramOutput as ProgramOutput
import Test.Spec (Spec, around, describe, it)
import Test.Utils (createCommit, unsafeNonEmptyString, withGitRepo)
import Test.Spec.Assertions (shouldEqual)

time ∷ Time
time = unsafePartial $ fromJust $ do
  hours ← toEnum 0
  minutes ← toEnum 0
  seconds ← toEnum 0
  millis ← toEnum 0
  pure $ Time.Time hours minutes seconds millis

date ∷ Date
date = unsafePartial $ fromJust $ do
  year ← toEnum 2000
  day ← toEnum 1
  Date.exactDate year Date.January day

spec ∷ Spec Unit
spec = describe "Git" do
  describe "execute" do
    markCommitAndGetLastCommitSpec
    markCommitAndRenderJsonSpec
    renderNoStagesJsonSpec

renderNoStagesJsonSpec ∷ Spec Unit
renderNoStagesJsonSpec = around withGitRepo do
  it "renders two commits with no stages in JSON" $
    \gitDirPath → do
      let
        createExampleCommit message =
          createCommit
            gitDirPath
            { authorName: "user1"
            , committerName: "user2"
            , date: DateTime date time
            , message
            }

      commitRef1 /\ commitInfo1 ← createExampleCommit "commit1"
      commitRef2 /\ commitInfo2 ← createExampleCommit "commit2"

      let
        expected = ProgramOutput.JSON $ encodeJson $ Repo $
          fromFoldable
            [ { info: commitInfo2
              , passedStages: Nil
              , ref: commitRef2
              }
            , { info: commitInfo1
              , passedStages: Nil
              , ref: commitRef1
              }
            ]

      actual ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        (Render $ RenderOptions { format: JSON })

      actual `shouldEqual` expected

markCommitAndRenderJsonSpec ∷ Spec Unit
markCommitAndRenderJsonSpec = around withGitRepo do
  it "marks a commit and renders two commits in JSON" $
    \gitDirPath → do
      let
        createExampleCommit message =
          createCommit
            gitDirPath
            { authorName: "user1"
            , committerName: "user2"
            , date: DateTime date time
            , message
            }

      commitRef1 /\ commitInfo1 ← createExampleCommit "commit1"
      commitRef2 /\ commitInfo2 ← createExampleCommit "commit2"

      let
        stageNameToMark = "one"
        expected = ProgramOutput.JSON $ encodeJson $ Repo $
          fromFoldable
            [ { info: commitInfo2
              , passedStages: Nil
              , ref: commitRef2
              }
            , { info: commitInfo1
              , passedStages: fromFoldable
                  [ CIStage $ unsafeNonEmptyString stageNameToMark ]
              , ref: commitRef1
              }
            ]

      output ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        ( MarkCommit $ MarkCommitOptions
            { ciStage: CIStage $ unsafeNonEmptyString stageNameToMark
            , commitRef: commitRef1
            }
        )

      actual ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        (Render $ RenderOptions { format: JSON })

      output `shouldEqual`
        ( ProgramOutput.Text $ "Marking commit " <> asHex commitRef1
            <> " with CI stage '"
            <> stageNameToMark
            <> "'"
        )

      actual `shouldEqual` expected

markCommitAndGetLastCommitSpec ∷ Spec Unit
markCommitAndGetLastCommitSpec = around withGitRepo do
  it "marks a commit and gets the last commit" $
    \gitDirPath → do
      let
        createExampleCommit message =
          createCommit
            gitDirPath
            { authorName: "user1"
            , committerName: "user2"
            , date: DateTime date time
            , message
            }

      commitRef1 /\ _ ← createExampleCommit "commit1"
      _ ← createExampleCommit "commit2"

      let
        stageNameToMark = "one"
        expected = ProgramOutput.Text $ asHex commitRef1

      output ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        ( MarkCommit $ MarkCommitOptions
            { ciStage: CIStage $ unsafeNonEmptyString stageNameToMark
            , commitRef: commitRef1
            }
        )

      actual ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        ( GetLast $ GetLastOptions
            { ciStages: fromFoldable
                [ CIStage $ unsafeNonEmptyString stageNameToMark ]
            }
        )

      output `shouldEqual`
        ( ProgramOutput.Text $ "Marking commit " <> asHex commitRef1
            <> " with CI stage '"
            <> stageNameToMark
            <> "'"
        )

      actual `shouldEqual` expected
