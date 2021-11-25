module Test.CiToolkit.Version.Calendar (spec) where

import Prelude

import CiToolkit.Common.CI (Repo(Repo))
import CiToolkit.Common.Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , Committer(Committer)
  , Tree(Tree)
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeTreeRef
  )
import CiToolkit.Common.Git.Commit.UserInfo
  ( Timestamp(Timestamp)
  , TimestampInstant(TimestampInstant)
  , UserInfo(UserInfo)
  , Username(Username)
  , unsafeEmail
  , unsafeTimestampTimezone
  )
import CiToolkit.Common.Utils (unsafeDate, unsafeTimeFromHours)
import CiToolkit.Version.Calendar
  ( formatCalendarVersion
  , showCalendarVersion
  )
import Data.Date (Date)
import Data.DateTime (DateTime(DateTime))
import Data.DateTime.Instant (fromDateTime)
import Data.List (fromFoldable)
import Data.String.NonEmpty as NES
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(Proxy))

spec ∷ Spec Unit
spec = describe "Calendar" do
  formatCalendarVersionSpec
  showCalendarVersionSpec

exampleCommitInfo
  ∷ Date
  → CommitInfo
exampleCommitInfo date =
  let
    timestamp = Timestamp
      { instant: TimestampInstant $ fromDateTime $
          DateTime date (unsafeTimeFromHours 0)
      , timezone: unsafeTimestampTimezone 0
      }
  in
    CommitInfo
      { author: Author $ UserInfo
          { email: unsafeEmail "user1@email.com"
          , timestamp
          , username: Username $ NES.nes (Proxy ∷ Proxy "user1")
          }
      , committer: Committer $ UserInfo
          { email: unsafeEmail "user2@email.com"
          , timestamp
          , username: Username $ NES.nes (Proxy ∷ Proxy "user2")
          }
      , message: unsafeCommitMessage "commit message"
      , parents: mempty
      , tree: Tree $ unsafeTreeRef "dummy"
      }

formatCalendarVersionSpec ∷ Spec Unit
formatCalendarVersionSpec = describe "formatCalendarVersion" do
  it "formats date properly" do
    let
      expected = "2020.05.01-2"
      actual = formatCalendarVersion
        (unsafeDate { year: 2020, month: 5, day: 1 })
        2
    actual `shouldEqual` expected

showCalendarVersionSpec ∷ Spec Unit
showCalendarVersionSpec = describe "showCalendarVersion" do
  it "calculates version properly" do
    let
      commitRef1 = unsafeCommitRef
        "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
      commitRef2 = unsafeCommitRef
        "f6e6e2206876c8ecc4479ac33b022fcea587d7c0"
      commitRef3 = unsafeCommitRef
        "5d7efdc1a20986a2b2152809f741be820bfcf007"

      repo = Repo $ fromFoldable
        [ { info: exampleCommitInfo $
              unsafeDate { year: 2020, month: 5, day: 1 }
          , passedStages: mempty
          , ref: commitRef1
          , tags: mempty
          }
        , { info: exampleCommitInfo $
              unsafeDate { year: 2020, month: 5, day: 2 }
          , passedStages: mempty
          , ref: commitRef2
          , tags: mempty
          }
        , { info: exampleCommitInfo $
              unsafeDate { year: 2020, month: 5, day: 2 }
          , passedStages: mempty
          , ref: commitRef3
          , tags: mempty
          }
        ]

      expected =
        [ pure "2020.05.01-1"
        , pure "2020.05.02-1"
        , pure "2020.05.02-2"
        ]

      actual = showCalendarVersion repo <$>
        [ commitRef1, commitRef2, commitRef3 ]

    actual `shouldEqual` expected
