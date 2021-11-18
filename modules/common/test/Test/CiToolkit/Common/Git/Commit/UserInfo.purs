module Test.CiToolkit.Common.Git.Commit.UserInfo (spec) where

import Prelude

import CiToolkit.Common.Git.Commit.UserInfo
  ( UserInfo(UserInfo)
  , Username(Username)
  , emailParser
  , timestampParser
  , unsafeEmail
  , unsafeTimestamp
  , usernameParser
  )
import CiToolkit.Common.Git.Object (gitObjectParser)
import CiToolkit.Common.Utils (unsafeNonEmptyString)
import Test.CiToolkit.Common.TestUtils
  ( genEmail
  , genTimestamp
  , genUsername
  , quickCheckGitObjectComponent
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)

spec ∷ Spec Unit
spec = describe "Git.Commit.UserInfo" do
  emailParserSpec
  timestampParserSpec
  userInfoParserSpec
  usernameParserSpec

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

timestampParserSpec ∷ Spec Unit
timestampParserSpec = describe "timestampParser" do

  it "parses a specific valid timestamp string" do
    let
      s = "0123456789 +0500"
      expected = pure $ unsafeTimestamp { ins: 123456789, tz: 5 }
      actual = runParser gitObjectParser s

    actual `shouldEqual` expected

  it "parses any valid timestamp string" do
    quickCheckGitObjectComponent genTimestamp timestampParser

userInfoParserSpec ∷ Spec Unit
userInfoParserSpec = describe "userInfo" do
  it "parses a specific valid user info string" do
    let
      s =
        "firstname lastname <firstname.lastname@example.com> 1234567890 +0500"
      expected = pure $ UserInfo
        { email: unsafeEmail "firstname.lastname@example.com"
        , timestamp: unsafeTimestamp { ins: 1234567890, tz: 5 }
        , username: Username $ unsafeNonEmptyString "firstname lastname"
        }
      actual = runParser gitObjectParser s

    actual `shouldEqual` expected

  it "parses any valid username string" do
    quickCheckGitObjectComponent genUsername usernameParser

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
