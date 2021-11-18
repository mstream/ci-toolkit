module Test.CiToolkit.Common.Git.Commit (spec) where

import Prelude

import CiToolkit.Common.Git.Commit
  ( Author(Author)
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
  , Notes(Notes)
  , Tree(Tree)
  , commitInfoParser
  , commitLinesParser
  , commitMessageParser
  , commitRefParser
  , commitRefsParser
  , notesParser
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeTreeRef
  )
import CiToolkit.Common.Git.Commit.UserInfo
  ( UserInfo(UserInfo)
  , Username(Username)
  , timestampParser
  , unsafeEmail
  , unsafeTimestamp
  , usernameParser
  )
import CiToolkit.Common.Git.Object (gitObjectParser)
import CiToolkit.Common.Utils (unsafeNonEmptyString)
import Data.List (fromFoldable)
import Data.String.NonEmpty as NES
import Test.CiToolkit.Common.TestUtils
  ( genCommitObject
  , genTimestamp
  , genUsername
  , quickCheckGitObjectComponent
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)
import Type.Proxy (Proxy(Proxy))

spec ∷ Spec Unit
spec = describe "Git.Commit" do
  commitInfoParserSpec
  commitLinesParserSpec
  commitMessageParserSpec
  commitRefParserSpec
  commitRefsParserSpec
  notesParserSpec

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
              , timestamp: unsafeTimestamp { ins: 1111111111, tz: -5 }
              , username: Username $ NES.nes (Proxy ∷ Proxy "user1")
              }
          , committer: Committer $ UserInfo
              { email: unsafeEmail "user2@email.com"
              , timestamp: unsafeTimestamp { ins: 123456789, tz: 5 }
              , username: Username $ NES.nes (Proxy ∷ Proxy "user2")
              }
          , message: unsafeCommitMessage "commit message"
          , parents: mempty
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
      timestamp = unsafeTimestamp { ins: 1111111111, tz: -5 }
      expected = pure $ fromFoldable
        [ AuthorLine $ Author $ UserInfo
            { email: unsafeEmail "user1@email.com"
            , timestamp
            , username: Username $ NES.nes (Proxy ∷ Proxy "user1")
            }
        , CommitterLine $ Committer $ UserInfo
            { email: unsafeEmail "user2@email.com"
            , timestamp
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
