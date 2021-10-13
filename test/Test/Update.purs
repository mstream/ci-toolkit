module Test.Update (spec) where

import Prelude

import CI (CIStage(CIStage), Repo(Repo))
import Data.List (fromFoldable)
import Data.Maybe (Maybe(Nothing))
import Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , Committer(Committer)
  , Timestamp(Timestamp)
  , UserInfo(UserInfo)
  , Username(Username)
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimezone
  )
import Update (Update(MarkWithCIStage), markCommit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (unsafeInstantFromSeconds, unsafeNonEmptyString)

spec ∷ Spec Unit
spec = describe "Update" do
  markCommitSpec

dummyCommitInfo ∷ CommitInfo
dummyCommitInfo = CommitInfo
  { author: Author $ UserInfo
      { email: unsafeEmail "user1@email.com"
      , timestamp:
          Timestamp $ unsafeInstantFromSeconds 1111111111
      , timezone: unsafeTimezone (-5)
      , username: Username $ unsafeNonEmptyString "user1"
      }
  , committer: Committer $ UserInfo
      { email: unsafeEmail "user2@email.com"
      , timestamp: Timestamp $ unsafeInstantFromSeconds 123456789
      , timezone: unsafeTimezone 5
      , username: Username $ unsafeNonEmptyString "user2"
      }
  , message: unsafeCommitMessage "commit message"
  }

markCommitSpec ∷ Spec Unit
markCommitSpec = describe "markCommit" do
  let
    commitRef1 = unsafeCommitRef
      "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
    commitRef2 = unsafeCommitRef
      "f6e6e2206876c8ecc4479ac33b022fcea587d7c0"
    ciStageToMark = CIStage $ unsafeNonEmptyString "two"
    repo = Repo $ fromFoldable
      [ { info: dummyCommitInfo
        , passedStages: fromFoldable
            [ CIStage $ unsafeNonEmptyString "one" ]
        , ref: commitRef1
        }
      , { info: dummyCommitInfo
        , passedStages: fromFoldable
            [ CIStage $ unsafeNonEmptyString "one"
            , CIStage $ unsafeNonEmptyString "two"
            ]
        , ref: commitRef2
        }
      ]

  it "marks the existing commit without the stage already marked" do
    let
      expected = pure $ MarkWithCIStage commitRef1 ciStageToMark
      actual = markCommit
        ciStageToMark
        commitRef1
        repo

    actual `shouldEqual` expected

  it "does not mark the existing commit with the stage already marked"
    do
      let
        expected = Nothing
        actual = markCommit
          ciStageToMark
          commitRef2
          repo

      actual `shouldEqual` expected

  it "does not mark a non-existent commit" do
    let
      expected = Nothing
      actual = markCommit
        ciStageToMark
        (unsafeCommitRef "non-existent")
        repo

    actual `shouldEqual` expected
