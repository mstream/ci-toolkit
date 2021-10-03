module Test.Git.Commit (spec) where

import Prelude

import Data.List (fromFoldable)
import Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , Committer(Committer)
  , Notes(Notes)
  , commitInfoParser
  , commitRefParser
  , commitRefsParser
  , notesParser
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimestamp
  , unsafeTimezone
  , unsafeUser
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)

spec ∷ Spec Unit
spec = describe "Git.Commit" do
  describe "commitInfoParser" do
    it "parses a valid commit info string" do
      let
        s = "author user1 <user1@email.com> 1111111111 -0500"
          <> "\n"
          <>
            "committer user2 <user2@email.com> 0123456789 +0500"
      let
        expected =
          pure $ CommitInfo
            { author: Author
                { email: unsafeEmail "user1@email.com"
                , timestamp: unsafeTimestamp 1111111111
                , timezone: unsafeTimezone (-5)
                , user: unsafeUser "user1"
                }
            , committer: Committer
                { email: unsafeEmail "user2@email.com"
                , timestamp: unsafeTimestamp 123456789
                , timezone: unsafeTimezone 5
                , user: unsafeUser "user2"
                }
            }
      let actual = runParser commitInfoParser s
      actual `shouldEqual` expected
  describe "commitRefParser" do
    it "parses a valid commit ref string" do
      let
        s = "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
      let
        expected =
          pure $ unsafeCommitRef
            "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"
      let actual = runParser commitRefParser s
      actual `shouldEqual` expected
  describe "commitRefsParser" do
    it "parses a valid commit refs string" do
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
      let actual = runParser commitRefsParser s
      actual `shouldEqual` expected
  describe "notesParser" do
    it "parses a valid notes string" do
      let
        s = "aaa"
          <> "\n"
          <> "bbb"
          <> "\n"
          <> "ccc"
      let
        expected =
          pure $ Notes $ fromFoldable [ "aaa", "bbb", "ccc" ]
      let actual = runParser notesParser s
      actual `shouldEqual` expected
