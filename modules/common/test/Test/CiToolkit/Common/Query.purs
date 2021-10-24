module Test.CiToolkit.Common.Query (spec) where

import Prelude

import CiToolkit.Common.CI (CIStage(CIStage), Repo(Repo))
import CiToolkit.Common.Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , CommitMessage
  , CommitParent
  , Committer(Committer)
  , Timestamp(Timestamp)
  , Tree(Tree)
  , UserInfo(UserInfo)
  , Username(Username)
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimezone
  , unsafeTreeRef
  )
import CiToolkit.Common.Query (findLastCommit)
import Data.List (List(Nil), fromFoldable)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Test.CiToolkit.Common.Utils
  ( unsafeInstantFromSeconds
  , unsafeNonEmptyString
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(Proxy))

spec ∷ Spec Unit
spec = describe "Query" do
  findLastCommitSpec

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
      , timestamp:
          Timestamp $ unsafeInstantFromSeconds 1111111111
      , timezone: unsafeTimezone (-5)
      , username: Username $ NES.nes (Proxy ∷ Proxy "user1")
      }
  , committer: Committer $ UserInfo
      { email: unsafeEmail "user2@email.com"
      , timestamp: Timestamp $ unsafeInstantFromSeconds 123456789
      , timezone: unsafeTimezone 5
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

    let
      expected = pure $ commitRef2 /\ commitInfo2
      actual = findLastCommit
        ( fromFoldable
            [ CIStage $ unsafeNonEmptyString "one"
            , CIStage $ unsafeNonEmptyString "two"
            ]
        )
        repo

    actual `shouldEqual` expected
