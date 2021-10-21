module Test.CI (spec) where

import Prelude

import CI
  ( CIStage(CIStage)
  , CIStagePrefix(CIStagePrefix)
  , Repo(Repo)
  , loadRepo
  )
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
import Test.Spec (Spec, around, describe, it)
import Test.Utils
  ( appendNotes
  , createCommit
  , unsafeNonEmptyString
  , withGitRepo
  )
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
  describe "loadRepo" do
    around withGitRepo do
      it "builds a repository model" $ \gitDirPath → do
        let
          testCommitInfo parentRef message =
            { authorName: "user1"
            , committerName: "user2"
            , date: DateTime date time
            , message
            , parentRef
            }

          createExampleCommit parentRef message =
            createCommit
              gitDirPath
              (testCommitInfo parentRef message)

          appendExampleNotes commitRef message =
            appendNotes
              gitDirPath
              (testCommitInfo Nothing message)
              commitRef

        refs1 /\ commitInfo1 ← createExampleCommit
          Nothing
          "commit1"

        refs2 /\ commitInfo2 ← createExampleCommit
          (pure refs1.commitRef)
          "commit2"

        appendExampleNotes
          refs1.commitRef
          "aaa\nbbb\n"

        appendExampleNotes
          refs2.commitRef
          "aaa\nci-one\nbbb\nci-two\nccc"

        let
          expected = Repo $ fromFoldable
            [ { info: commitInfo2
              , passedStages: fromFoldable
                  [ CIStage $ unsafeNonEmptyString "one"
                  , CIStage $ unsafeNonEmptyString "two"
                  ]
              , ref: refs2.commitRef
              }
            , { info: commitInfo1
              , passedStages: Nil
              , ref: refs1.commitRef
              }
            ]

        actual ← loadRepo
          gitDirPath
          (CIStagePrefix $ unsafeNonEmptyString "ci-")

        actual `shouldEqual` expected
