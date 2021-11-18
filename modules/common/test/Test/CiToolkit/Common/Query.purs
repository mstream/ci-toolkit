module Test.CiToolkit.Common.Query (spec) where

import Prelude

import CiToolkit.Common.CI (CIStage(CIStage), Repo(Repo))
import CiToolkit.Common.Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , CommitMessage
  , CommitParent
  , Committer(Committer)
  , Tree(Tree)
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeTreeRef
  )
import CiToolkit.Common.Git.Commit.UserInfo
  ( UserInfo(UserInfo)
  , Username(Username)
  , unsafeEmail
  , unsafeTimestamp
  )
import CiToolkit.Common.Query (findLastCommit, sortCommitsByTimestamp)
import CiToolkit.Common.Utils (unsafeNonEmptyString)
import Data.List (List(Nil), fromFoldable)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(Proxy))

spec ∷ Spec Unit
spec = describe "Query" do
  findLastCommitSpec
  sortCommitsByTimestampSpec

dummyCommitInfoData ∷
  { author ∷ Author
  , committer ∷ Committer
  , message ∷ CommitMessage
  , parents ∷ List CommitParent
  , tree ∷ Tree
  }
dummyCommitInfoData =
  { author: Author $ UserInfo
      { email: unsafeEmail "user1@email.com"
      , timestamp: unsafeTimestamp { ins: 1, tz: 0 }
      , username: Username $ NES.nes (Proxy ∷ Proxy "user1")
      }
  , committer: Committer $ UserInfo
      { email: unsafeEmail "user2@email.com"
      , timestamp: unsafeTimestamp { ins: 2, tz: 0 }
      , username: Username $ NES.nes (Proxy ∷ Proxy "user2")
      }
  , message: unsafeCommitMessage "commit message"
  , parents: Nil
  , tree: Tree $ unsafeTreeRef "dummy"
  }

findLastCommitSpec ∷ Spec Unit
findLastCommitSpec = describe "findLastCommit" do
  it "retrieves the last commit which passed given CI stages" do
    let
      commitRef1 = unsafeCommitRef
        "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
      commitRef2 = unsafeCommitRef
        "f6e6e2206876c8ecc4479ac33b022fcea587d7c0"
      commitRef3 = unsafeCommitRef
        "5d7efdc1a20986a2b2152809f741be820bfcf007"
      commitInfo1 = CommitInfo $ dummyCommitInfoData
        { message = unsafeCommitMessage "commit1" }
      commitInfo2 = CommitInfo $ dummyCommitInfoData
        { message = unsafeCommitMessage "commit2" }
      commitInfo3 = CommitInfo $ dummyCommitInfoData
        { message = unsafeCommitMessage "commit3" }

      repo = Repo $ fromFoldable
        [ { info: commitInfo3
          , passedStages: fromFoldable
              [ CIStage $ unsafeNonEmptyString "one"
              ]
          , ref: commitRef3
          , tags: Nil
          }
        , { info: commitInfo2
          , passedStages: fromFoldable
              [ CIStage $ unsafeNonEmptyString "one"
              , CIStage $ unsafeNonEmptyString "two"
              ]
          , ref: commitRef2
          , tags: Nil
          }
        , { info: commitInfo1
          , passedStages: fromFoldable
              [ CIStage $ unsafeNonEmptyString "one"
              , CIStage $ unsafeNonEmptyString "two"
              ]
          , ref: commitRef1
          , tags: Nil
          }
        ]

      expected = pure $ commitRef2 /\ commitInfo2
      actual = findLastCommit
        ( fromFoldable
            [ CIStage $ unsafeNonEmptyString "one"
            , CIStage $ unsafeNonEmptyString "two"
            ]
        )
        repo

    actual `shouldEqual` expected

sortCommitsByTimestampSpec ∷ Spec Unit
sortCommitsByTimestampSpec = describe "sortCommitsByTimestamp" do
  it "sorts commits from the oldest to the newest" do
    let
      testCommitInfo epochSeconds =
        CommitInfo $ dummyCommitInfoData
          { committer = Committer $ UserInfo
              { email: unsafeEmail "user1@email.com"
              , timestamp: unsafeTimestamp { ins: epochSeconds, tz: 0 }
              , username: Username $ NES.nes (Proxy ∷ Proxy "user1")
              }
          }

      commitRef1 = unsafeCommitRef
        "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
      commitRef2 = unsafeCommitRef
        "f6e6e2206876c8ecc4479ac33b022fcea587d7c0"
      commitRef3 = unsafeCommitRef
        "5d7efdc1a20986a2b2152809f741be820bfcf007"
      commitRef4 = unsafeCommitRef
        "2258c2abbfef03ecd38dca285df0e0c0433c71b1"

      commitInfo1 = testCommitInfo 1
      commitInfo2 = testCommitInfo 2
      commitInfo3 = testCommitInfo 3
      commitInfo4 = testCommitInfo 4

      repo = Repo $ fromFoldable
        [ { info: commitInfo3
          , passedStages: Nil
          , ref: commitRef3
          , tags: Nil
          }
        , { info: commitInfo2
          , passedStages: Nil
          , ref: commitRef2
          , tags: Nil
          }
        , { info: commitInfo4
          , passedStages: Nil
          , ref: commitRef4
          , tags: Nil
          }
        , { info: commitInfo1
          , passedStages: Nil
          , ref: commitRef1
          , tags: Nil
          }
        ]

      expected = Repo $ fromFoldable
        [ { info: commitInfo1
          , passedStages: Nil
          , ref: commitRef1
          , tags: Nil
          }
        , { info: commitInfo2
          , passedStages: Nil
          , ref: commitRef2
          , tags: Nil
          }
        , { info: commitInfo3
          , passedStages: Nil
          , ref: commitRef3
          , tags: Nil
          }
        , { info: commitInfo4
          , passedStages: Nil
          , ref: commitRef4
          , tags: Nil
          }
        ]

      actual = sortCommitsByTimestamp repo

    actual `shouldEqual` expected
