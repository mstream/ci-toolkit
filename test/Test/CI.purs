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
import Data.Maybe (fromJust)
import Data.String.NonEmpty (nes)
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
import Type.Proxy (Proxy(Proxy))

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
          testCommitInfo message =
            { authorName: "user1"
            , committerName: "user2"
            , date: DateTime date time
            , message
            }

          createExampleCommit message =
            createCommit
              gitDirPath
              (testCommitInfo message)

          appendExampleNotes commitRef message =
            appendNotes
              gitDirPath
              (testCommitInfo message)
              commitRef

        commitRef1 /\ commitInfo1 ← createExampleCommit "commit1"
        commitRef2 /\ commitInfo2 ← createExampleCommit "commit2"
        appendExampleNotes commitRef1 "aaa\nbbb\n"
        appendExampleNotes commitRef2 "aaa\nci-one\nbbb\nci-two\nccc"

        let
          expected = Repo $ fromFoldable
            [ { info: commitInfo2
              , passedStages: fromFoldable
                  [ CIStage $ unsafeNonEmptyString "one"
                  , CIStage $ unsafeNonEmptyString "two"
                  ]
              , ref: commitRef2
              }
            , { info: commitInfo1
              , passedStages: Nil
              , ref: commitRef1
              }
            ]

        actual ← loadRepo
          gitDirPath
          (CIStagePrefix $ unsafeNonEmptyString "ci-")

        actual `shouldEqual` expected
