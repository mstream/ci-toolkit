module Test.Git (spec) where

import Prelude

import Data.Date (Date)
import Data.Date as Date
import Data.DateTime (DateTime(DateTime))
import Data.Enum (toEnum)
import Data.List (fromFoldable)
import Data.Maybe (fromJust)
import Data.Time (Time)
import Data.Time as Time
import Git (getCommitRefs)
import Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , Committer(Committer)
  , Timestamp(Timestamp)
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimezone
  , unsafeUser
  )
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, around, describe, it)
import Test.Utils (createCommit, unsafeInstantFromSeconds, withGitRepo)
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
  describe "getCommitRefs" do
    around withGitRepo do
      it "retrieves all commit refs" $ \gitDirPath → do
        let
          createExampleCommit message =
            createCommit
              gitDirPath
              { authorName: "user1"
              , committerName: "user2"
              , date: DateTime date time
              , message
              }

        void $ createExampleCommit "commit1"
        void $ createExampleCommit "commit2"

        let
          expected = fromFoldable
            [ unsafeCommitRef "8a5b31da784c58bbe495381b12e0a8785ae66ddd"
            , unsafeCommitRef "951f1b6d05208893909fcc21badbc901ee8543b4"
            ]

        actual ← getCommitRefs gitDirPath
        actual `shouldEqual` expected
