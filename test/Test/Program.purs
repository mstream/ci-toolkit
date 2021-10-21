module Test.Program (spec) where

import Prelude

import CI (CIStage(CIStage), CIStagePrefix(CIStagePrefix), Repo(Repo))
import Data.Argonaut (encodeJson)
import Data.Date (Date)
import Data.Date as Date
import Data.DateTime (DateTime(DateTime))
import Data.Enum (toEnum)
import Data.List (List(Nil), fromFoldable)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Time (Time)
import Data.Time as Time
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Print (showToHuman)
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
spec = describe "Program" do
  describe "execute" do
    markCommitAndGetLastCommitSpec
    markCommitAndRenderJsonSpec
    markCommitWithTheSameStageTwiceSpec
    renderNoStagesJsonSpec

renderNoStagesJsonSpec ∷ Spec Unit
renderNoStagesJsonSpec = around withGitRepo do
  it "renders two commits with no stages in JSON" $
    \gitDirPath → do
      let
        createExampleCommit parentRef message =
          createCommit
            gitDirPath
            { authorName: "user1"
            , committerName: "user2"
            , date: DateTime date time
            , message
            , parentRef
            }

      refs1 /\ commitInfo1 ← createExampleCommit
        Nothing
        "commit1"

      refs2 /\ commitInfo2 ← createExampleCommit
        (pure refs1.commitRef)
        "commit2"

      let
        expected = ProgramOutput.JSON $ encodeJson $ Repo $
          fromFoldable
            [ { info: commitInfo2
              , passedStages: Nil
              , ref: refs2.commitRef
              }
            , { info: commitInfo1
              , passedStages: Nil
              , ref: refs1.commitRef
              }
            ]

      actual ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , ciStages: Nil
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        ( Render $ RenderOptions
            { format: JSON, limit: 0 }
        )

      actual `shouldEqual` expected

markCommitAndRenderJsonSpec ∷ Spec Unit
markCommitAndRenderJsonSpec = around withGitRepo do
  it "marks a commit and renders two commits in JSON" $
    \gitDirPath → do
      let
        createExampleCommit parentRef message =
          createCommit
            gitDirPath
            { authorName: "user1"
            , committerName: "user2"
            , date: DateTime date time
            , message
            , parentRef
            }

      refs1 /\ commitInfo1 ← createExampleCommit
        Nothing
        "commit1"

      refs2 /\ commitInfo2 ← createExampleCommit
        (pure refs1.commitRef)
        "commit2"

      let
        stageNameToMark = "one"
        expected = ProgramOutput.JSON $ encodeJson $ Repo $
          fromFoldable
            [ { info: commitInfo2
              , passedStages: Nil
              , ref: refs2.commitRef
              }
            , { info: commitInfo1
              , passedStages: fromFoldable
                  [ CIStage $ unsafeNonEmptyString stageNameToMark ]
              , ref: refs1.commitRef
              }
            ]

      output ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , ciStages: Nil
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        ( MarkCommit $ MarkCommitOptions
            { ciStage: CIStage $ unsafeNonEmptyString stageNameToMark
            , commitRef: refs1.commitRef
            }
        )

      actual ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , ciStages: fromFoldable []
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        ( Render $ RenderOptions
            { format: JSON, limit: 0 }
        )

      output `shouldEqual`
        ( ProgramOutput.Text $ "Marking commit "
            <> (showToHuman unit refs1.commitRef)
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
        createExampleCommit parentRef message =
          createCommit
            gitDirPath
            { authorName: "user1"
            , committerName: "user2"
            , date: DateTime date time
            , message
            , parentRef
            }

      refs1 /\ _ ← createExampleCommit
        Nothing
        "commit1"

      _ ← createExampleCommit
        (pure refs1.commitRef)
        "commit2"

      let
        stageNameToMark = "one"
        expected = ProgramOutput.Text $ showToHuman unit refs1.commitRef

      output ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , ciStages: Nil
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        ( MarkCommit $ MarkCommitOptions
            { ciStage: CIStage $ unsafeNonEmptyString stageNameToMark
            , commitRef: refs1.commitRef
            }
        )

      actual ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , ciStages: fromFoldable
                [ CIStage $ unsafeNonEmptyString stageNameToMark ]
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        ( GetLast $ GetLastOptions {}
        )

      output `shouldEqual`
        ( ProgramOutput.Text $ "Marking commit "
            <> (showToHuman unit refs1.commitRef)
            <> " with CI stage '"
            <> stageNameToMark
            <> "'"
        )

      actual `shouldEqual` expected

markCommitWithTheSameStageTwiceSpec ∷ Spec Unit
markCommitWithTheSameStageTwiceSpec = around withGitRepo do
  it "marks a commit with the same stage name twice" $
    \gitDirPath → do
      let
        createExampleCommit parentRef message =
          createCommit
            gitDirPath
            { authorName: "user1"
            , committerName: "user2"
            , date: DateTime date time
            , message
            , parentRef
            }

      refs1 /\ _ ← createExampleCommit
        Nothing
        "commit1"

      void $ createExampleCommit
        (pure refs1.commitRef)
        "commit2"

      let
        stageNameToMark = "one"

      void $ execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , ciStages: Nil
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        ( MarkCommit $ MarkCommitOptions
            { ciStage: CIStage $ unsafeNonEmptyString stageNameToMark
            , commitRef: refs1.commitRef
            }
        )

      output ← execute $ ProgramInput
        ( CommonOptions
            { ciPrefix: CIStagePrefix $ unsafeNonEmptyString "ci-"
            , ciStages: Nil
            , dryRun: false
            , gitDirectory: gitDirPath
            , isVerbose: false
            }
        )
        ( MarkCommit $ MarkCommitOptions
            { ciStage: CIStage $ unsafeNonEmptyString stageNameToMark
            , commitRef: refs1.commitRef
            }
        )

      output `shouldEqual`
        ( ProgramOutput.Text $ "Commit '"
            <> (showToHuman unit refs1.commitRef)
            <> "' is already marked with CI stage '"
            <> stageNameToMark
            <> "'"
        )
