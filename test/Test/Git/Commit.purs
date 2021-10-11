module Test.Git.Commit (spec) where

import Prelude

import Data.Array.NonEmpty (fromNonEmpty)
import Data.Char.Gen (genAsciiChar)
import Data.List (fromFoldable)
import Data.NonEmpty ((:|))
import Data.String (joinWith)
import Data.String.Gen (genString)
import Data.String.NonEmpty as NES
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Git.Commit
  ( class GitObjectComponent
  , Author(Author)
  , CommitInfo(CommitInfo)
  , CommitMessage(CommitMessage)
  , Committer(Committer)
  , Email
  , Notes(Notes)
  , Timestamp(Timestamp)
  , Timezone
  , UserInfo(UserInfo)
  , Username(Username)
  , commitInfoParser
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
  , usernameParser
  )
import Test.QuickCheck ((<?>), quickCheckGen')
import Test.QuickCheck.Gen (Gen, arrayOf1, elements, suchThat)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (unsafeInstantFromSeconds, unsafeNonEmptyString)
import Text.Parsing.StringParser (Parser, runParser)
import Type.Proxy (Proxy(Proxy))

spec ∷ Spec Unit
spec = describe "Git.Commit" do
  commitInfoParserSpec
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
        <>
          "committer user2 <user2@email.com> 0123456789 +0500"
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
          }
    let
      actual = runParser commitInfoParser commitObject

    actual `shouldEqual` expected
  it "parses any valid commit object" do
    quickCheckGitObjectComponent genCommitObject commitInfoParser

commitMessageParserSpec ∷ Spec Unit
commitMessageParserSpec = describe "commitMessageParser" do
  it "parses a valid message string" do
    let
      s = "aaa" <> "\n" <> "bbb" <> "\n\n" <> "commit message"
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
  in
    CommitInfo
      { author, committer, message: CommitMessage "commit message" }

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

  domain ← elements $ fromNonEmpty $
    "example1" :| [ "example2" ]

  tld ← elements $ fromNonEmpty $
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

    pure $ (actual == pure component) <?>
      ( "unable to parse the following component string:\n"
          <> "===\n>>>"
          <> componentString
          <> "<<<\n==="
          <> "Problem:\n"
          <> show actual
      )
