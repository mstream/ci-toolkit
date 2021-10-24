module Test.CiToolkit.Common.Git.Commit (spec) where

import Prelude

import CiToolkit.Common.Git.Commit
  ( class GitObjectComponent
  , Author(Author)
  , CommitInfo(CommitInfo)
  , CommitLine
      ( MessageLine
      , UnknownLine
      , ParentLine
      , CommitterLine
      , AuthorLine
      )
  , CommitMessage(CommitMessage)
  , CommitParent(CommitParent)
  , Committer(Committer)
  , Email
  , Notes(Notes)
  , Timestamp(Timestamp)
  , Timezone
  , Tree(Tree)
  , UserInfo(UserInfo)
  , Username(Username)
  , commitInfoParser
  , commitLinesParser
  , commitMessageParser
  , commitRefParser
  , commitRefsParser
  , emailParser
  , gitObjectParser
  , notesParser
  , showInGitObject
  , timestampParser
  , timezoneParser
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimezone
  , unsafeTreeRef
  , usernameParser
  )
import Data.Array.NonEmpty as NEA
import Data.Char.Gen (genAsciiChar)
import Data.List (List(Nil), fromFoldable)
import Data.NonEmpty ((:|))
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Gen (genString)
import Data.String.NonEmpty as NES
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.CiToolkit.Common.Utils
  ( toResult
  , unsafeInstantFromSeconds
  , unsafeNonEmptyString
  )
import Test.QuickCheck (quickCheckGen')
import Test.QuickCheck.Gen
  ( Gen
  , arrayOf
  , arrayOf1
  , elements
  , suchThat
  , vectorOf
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (Parser, runParser)
import Type.Proxy (Proxy(Proxy))

spec ∷ Spec Unit
spec = describe "Git.Commit" do
  commitInfoParserSpec
  commitLinesParserSpec
  commitMessageParserSpec
  commitRefParserSpec
  commitRefsParserSpec
  emailParserSpec
  notesParserSpec
  timestampParserSpec
  timezoneParserSpec
  userInfoParserSpec
  usernameParserSpec

commitInfoParserSpec ∷ Spec Unit
commitInfoParserSpec = describe "commitInfoParser" do
  it "parses a specific valid commit object" do
    let
      commitObject = "author user1 <user1@email.com> 1111111111 -0500"
        <> "\n"
        <> "committer user2 <user2@email.com> 0123456789 +0500"
        <> "\n"
        <> "tree b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
        <> "\n"
        <> "\n"
        <> "commit message"
    let
      expected =
        pure $ CommitInfo
          { author: Author $ UserInfo
              { email: unsafeEmail "user1@email.com"
              , timestamp:
                  Timestamp $ unsafeInstantFromSeconds 1111111111
              , timezone: unsafeTimezone (-5)
              , username: Username $ NES.nes (Proxy ∷ Proxy "user1")
              }
          , committer: Committer $ UserInfo
              { email: unsafeEmail "user2@email.com"
              , timestamp:
                  Timestamp $ unsafeInstantFromSeconds 123456789
              , timezone: unsafeTimezone 5
              , username: Username $ NES.nes (Proxy ∷ Proxy "user2")
              }
          , message: unsafeCommitMessage "commit message"
          , parents: Nil
          , tree: Tree $ unsafeTreeRef
              "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
          }
    let
      actual = runParser commitInfoParser commitObject

    actual `shouldEqual` expected
  it "parses any valid commit object" do
    quickCheckGitObjectComponent genCommitObject commitInfoParser

commitLinesParserSpec ∷ Spec Unit
commitLinesParserSpec = describe "commitLinesParser" do
  it "parses valid message lines" do
    let
      s = "author user1 <user1@email.com> 1111111111 -0500"
        <> "\n"
        <> "committer user2 <user2@email.com> 1111111111 -0500"
        <> "\n"
        <> "parent b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
        <> "\n"
        <> "parent f6e6e2206876c8ecc4479ac33b022fcea587d7c0"
        <> "\n"
        <> "\n"
        <> "commit line 1"
        <> "\n"
        <> "commit line 2"
        <> "\n"
        <> "commit line 3"
    let
      expected = pure $ fromFoldable
        [ AuthorLine $ Author $ UserInfo
            { email: unsafeEmail "user1@email.com"
            , timestamp:
                Timestamp $ unsafeInstantFromSeconds 1111111111
            , timezone: unsafeTimezone (-5)
            , username: Username $ NES.nes (Proxy ∷ Proxy "user1")
            }
        , CommitterLine $ Committer $ UserInfo
            { email: unsafeEmail "user2@email.com"
            , timestamp:
                Timestamp $ unsafeInstantFromSeconds 1111111111
            , timezone: unsafeTimezone (-5)
            , username: Username $ NES.nes (Proxy ∷ Proxy "user2")
            }
        , ParentLine $ CommitParent $ unsafeCommitRef
            "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
        , ParentLine $ CommitParent $ unsafeCommitRef
            "f6e6e2206876c8ecc4479ac33b022fcea587d7c0"
        , UnknownLine
        , MessageLine $ CommitMessage "commit line 1"
        , MessageLine $ CommitMessage "commit line 2"
        , MessageLine $ CommitMessage "commit line 3"
        ]
    let
      actual = runParser commitLinesParser s

    actual `shouldEqual` expected

commitMessageParserSpec ∷ Spec Unit
commitMessageParserSpec = describe "commitMessageParser" do
  it "parses a valid message string" do
    let
      s = "commit message"
    let
      expected = pure $ unsafeCommitMessage "commit message"
    let
      actual = runParser commitMessageParser s

    actual `shouldEqual` expected

commitRefParserSpec ∷ Spec Unit
commitRefParserSpec = describe "commitRefParser" do
  it "parses a valid commit ref string" do
    let
      s = "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
    let
      expected =
        pure $ unsafeCommitRef
          "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
    let
      actual = runParser commitRefParser s

    actual `shouldEqual` expected

commitRefsParserSpec ∷ Spec Unit
commitRefsParserSpec = describe "commitRefsParser" do
  it "parses a valid commit ref string" do
    let
      s = "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
        <> "\n"
        <> "f6e6e2206876c8ecc4479ac33b022fcea587d7c0"
        <> "\n"
    let
      expected =
        pure $ fromFoldable
          [ unsafeCommitRef "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
          , unsafeCommitRef "f6e6e2206876c8ecc4479ac33b022fcea587d7c0"
          ]
    let
      actual = runParser commitRefsParser s

    actual `shouldEqual` expected

emailParserSpec ∷ Spec Unit
emailParserSpec = describe "emailParser" do
  it "parses a specific valid email string" do
    let
      s = "aaa.bbb@example.com"
      expected = pure $ unsafeEmail s
      actual = runParser emailParser s

    actual `shouldEqual` expected

  it "parses any valid email string" do
    quickCheckGitObjectComponent genEmail emailParser

notesParserSpec ∷ Spec Unit
notesParserSpec = describe "notesParser" do
  it "parses a valid notes string" do
    let
      s = "aaa" <> "\n" <> "bbb" <> "\n" <> "ccc"
    let
      expected = pure $ Notes $ fromFoldable [ "aaa", "bbb", "ccc" ]
    let
      actual = runParser notesParser s

    actual `shouldEqual` expected

timezoneParserSpec ∷ Spec Unit
timezoneParserSpec = describe "timezoneParser" do

  it "parses a specific valid timezone string" do
    let
      s = "+0900"
      expected = pure $ unsafeTimezone 9
      actual = runParser gitObjectParser s

    actual `shouldEqual` expected

  it "parses any valid timezone string" do
    quickCheckGitObjectComponent genTimezone timezoneParser

timestampParserSpec ∷ Spec Unit
timestampParserSpec = describe "timestampParser" do

  it "parses a specific valid timestamp string" do
    let
      s = "0123456789"
      expected = pure $ Timestamp $ unsafeInstantFromSeconds 123456789
      actual = runParser gitObjectParser s

    actual `shouldEqual` expected

  it "parses any valid timestamp string" do
    quickCheckGitObjectComponent genTimestamp timestampParser

usernameParserSpec ∷ Spec Unit
usernameParserSpec = describe "usernameParser" do

  it "parses a specific valid username string" do
    let
      s = "0123456789"
      expected = pure $ Username $ unsafeNonEmptyString s
      actual = runParser gitObjectParser s

    actual `shouldEqual` expected

  it "parses any valid username string" do
    quickCheckGitObjectComponent genUsername usernameParser

userInfoParserSpec ∷ Spec Unit
userInfoParserSpec = describe "userInfo" do

  it "parses a specific valid user info string" do
    let
      s =
        "firstname lastname <firstname.lastname@example.com> 1234567890 +0500"
      expected = pure $ UserInfo
        { email: unsafeEmail "firstname.lastname@example.com"
        , timestamp: Timestamp $ unsafeInstantFromSeconds 1234567890
        , timezone: unsafeTimezone 5
        , username: Username $ unsafeNonEmptyString "firstname lastname"
        }
      actual = runParser gitObjectParser s

    actual `shouldEqual` expected

  it "parses any valid username string" do
    quickCheckGitObjectComponent genUsername usernameParser

genCommitObject ∷ Gen CommitInfo
genCommitObject = ado
  author ← genAuthor
  committer ← genCommitter
  parents ← fromFoldable <$> arrayOf
    (CommitParent <$> genGitObjectRef unsafeCommitRef)
  tree ← (Tree <$> genGitObjectRef unsafeTreeRef)
  in
    CommitInfo
      { author
      , committer
      , message: CommitMessage "commit message"
      , parents
      , tree
      }

genAuthor ∷ Gen Author
genAuthor = ado
  userInfo ← genUserInfo
  in
    Author userInfo

genCommitter ∷ Gen Committer
genCommitter = ado
  userInfo ← genUserInfo
  in
    Committer userInfo

genUserInfo ∷ Gen UserInfo
genUserInfo = ado
  username ← genUsername
  email ← genEmail
  timestamp ← genTimestamp
  timezone ← genTimezone
  in
    UserInfo { username, email, timestamp, timezone }

genGitObjectRef ∷ ∀ a. (String → a) → Gen a
genGitObjectRef refFromString = ado
  chars ← vectorOf 40 $ elements $ NEA.fromNonEmpty $ '0' :|
    [ '1'
    , '2'
    , '3'
    , '4'
    , '5'
    , '6'
    , '7'
    , '8'
    , '9'
    , 'a'
    , 'b'
    , 'c'
    , 'd'
    , 'e'
    , 'f'
    ]
  in refFromString $ fromCharArray chars

genTimestamp ∷ Gen Timestamp
genTimestamp =
  pure $ Timestamp $ unsafeInstantFromSeconds 1603432894

genTimezone ∷ Gen Timezone
genTimezone =
  pure $ unsafeTimezone 9

genEmail ∷ Gen Email
genEmail = do
  localPartSegments ← arrayOf1 genUsernameSegment

  let
    localPart = NES.joinWith1
      (unsafeNonEmptyString ".")
      localPartSegments

  domain ← elements $ NEA.fromNonEmpty $
    "example1" :| [ "example2" ]

  tld ← elements $ NEA.fromNonEmpty $
    "com" :| [ "org" ]

  pure $ unsafeEmail $ joinWith
    "@"
    [ NES.toString localPart, domain <> "." <> tld ]

genUsername ∷ Gen Username
genUsername = do
  nameSegments ← arrayOf1 genUsernameSegment

  let s = NES.joinWith1 (unsafeNonEmptyString " ") nameSegments

  pure $ Username s

genUsernameSegment ∷ Gen String
genUsernameSegment = genString $ genAsciiChar `suchThat` \c →
  c /= ' ' && c /= '<' && c /= '>' && c /= '\n'

quickCheckGitObjectComponent
  ∷ ∀ a
  . Eq a
  ⇒ GitObjectComponent a
  ⇒ Show a
  ⇒ Gen a
  → Parser a
  → Aff Unit
quickCheckGitObjectComponent generate parser =
  liftEffect $ quickCheckGen' 200 $ do
    component ← generate

    let
      componentString = showInGitObject component
      actual = runParser parser componentString

    pure $ toResult
      { componentString }
      { actual, expected: pure component }
