module Test.Git.Commit (spec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.List (fromFoldable)
import Effect.Exception (Error)
import Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , Committer(Committer)
  , Notes(Notes)
  , Timestamp(Timestamp)
  , commitInfoParser
  , commitMessageParser
  , commitRefParser
  , commitRefsParser
  , notesParser
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimezone
  , unsafeUser
  )
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (unsafeInstantFromSeconds)
import Text.Parsing.StringParser (runParser)

spec ∷ Spec Unit
spec = describe "Git.Commit" do
  commitInfoParserSpec
  commitMessageParserSpec
  commitRefParserSpec
  commitRefsParserSpec
  notesParserSpec

commitInfoParserSpec
  ∷ ∀ m1 m2. Monad m1 ⇒ MonadThrow Error m2 ⇒ SpecT m2 Unit m1 Unit
commitInfoParserSpec = describe "commitInfoParser" do
  it "parses a valid commit info string" do
    let
      s = "author user1 <user1@email.com> 1111111111 -0500"
        <> "\n"
        <>
          "committer user2 <user2@email.com> 0123456789 +0500"
        <> "\n"
        <> "\n"
        <> "commit message"
    let
      expected =
        pure $ CommitInfo
          { author: Author
              { email: unsafeEmail "user1@email.com"
              , timestamp:
                  Timestamp $ unsafeInstantFromSeconds 1111111111
              , timezone: unsafeTimezone (-5)
              , user: unsafeUser "user1"
              }
          , committer: Committer
              { email: unsafeEmail "user2@email.com"
              , timestamp:
                  Timestamp $ unsafeInstantFromSeconds 123456789
              , timezone: unsafeTimezone 5
              , user: unsafeUser "user2"
              }
          , message: unsafeCommitMessage "commit message"
          }
    let
      actual = runParser commitInfoParser s

    actual `shouldEqual` expected

commitMessageParserSpec
  ∷ ∀ m1 m2
  . Monad m1
  ⇒ MonadThrow Error m2
  ⇒ SpecT m2 Unit m1 Unit
commitMessageParserSpec = describe "commitMessageParser" do
  it "parses a valid message string" do
    let
      s = "aaa" <> "\n" <> "bbb" <> "\n\n" <> "commit message"
    let
      expected = pure $ unsafeCommitMessage "commit message"
    let
      actual = runParser commitMessageParser s

    actual `shouldEqual` expected

commitRefParserSpec
  ∷ ∀ m1 m2
  . Monad m1
  ⇒ MonadThrow Error m2
  ⇒ SpecT m2 Unit m1 Unit
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

commitRefsParserSpec
  ∷ ∀ m1 m2
  . Monad m1
  ⇒ MonadThrow Error m2
  ⇒ SpecT m2 Unit m1 Unit
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

notesParserSpec
  ∷ ∀ m1 m2. Monad m1 ⇒ MonadThrow Error m2 ⇒ SpecT m2 Unit m1 Unit
notesParserSpec = describe "notesParser" do
  it "parses a valid notes string" do
    let
      s = "aaa" <> "\n" <> "bbb" <> "\n" <> "ccc"
    let
      expected = pure $ Notes $ fromFoldable [ "aaa", "bbb", "ccc" ]
    let
      actual = runParser notesParser s

    actual `shouldEqual` expected
