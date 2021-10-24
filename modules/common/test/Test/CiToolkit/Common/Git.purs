module Test.CiToolkit.Common.Git (spec) where

import Prelude

import CiToolkit.Common.Git (getCommitRefs, getTags)
import CiToolkit.Common.Git.Commit (unsafeCommitRef)
import CiToolkit.Common.Git.Tag (unsafeTag)
import Data.Date (Date)
import Data.Date as Date
import Data.DateTime (DateTime(DateTime))
import Data.Enum (toEnum)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Time (Time)
import Data.Time as Time
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Test.CiToolkit.Common.Utils
  ( createCommit
  , createTag
  , withGitRepo
  )
import Test.Spec (Spec, around, describe, it)
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
  getCommitRefsSpec
  getTagsSpec

getCommitRefsSpec ∷ Spec Unit
getCommitRefsSpec = describe "getCommitRefs" do
  around withGitRepo do
    it "retrieves all commit refs" $ \gitDirPath → do
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
        expected = fromFoldable
          [ unsafeCommitRef "8a5b31da784c58bbe495381b12e0a8785ae66ddd"
          , unsafeCommitRef "951f1b6d05208893909fcc21badbc901ee8543b4"
          ]

      actual ← getCommitRefs gitDirPath
      actual `shouldEqual` expected

getTagsSpec ∷ Spec Unit
getTagsSpec = describe "getTags" do
  around withGitRepo do
    it "retrieves all tags" $ \gitDirPath → do
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

        createExampleTag = createTag gitDirPath

      refs1 /\ _ ← createExampleCommit
        Nothing
        "commit1"

      createExampleTag "tag_1a"
      createExampleTag "tag_1b"

      void $ createExampleCommit
        (pure refs1.commitRef)
        "commit2"

      createExampleTag "tag_2"

      let
        expected = fromFoldable
          [ unsafeTag "tag_1a"
          , unsafeTag "tag_1b"
          , unsafeTag "tag_2"
          ]

      actual ← getTags gitDirPath
      actual `shouldEqual` expected
