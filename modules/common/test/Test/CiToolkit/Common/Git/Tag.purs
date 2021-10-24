module Test.CiToolkit.Common.Git.Tag (spec) where

import Prelude

import CiToolkit.Common.Git.Tag (tagParser, tagsParser, unsafeTag)
import Data.List (fromFoldable)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)

spec ∷ Spec Unit
spec = describe "Git.Tag" do
  tagParserSpec
  tagsParserSpec

tagParserSpec ∷ Spec Unit
tagParserSpec = describe "tagParser" do
  it "parses a valid tag string" do
    let
      s = "v1.2.3"
      expected = pure $ unsafeTag "v1.2.3"
      actual = runParser tagParser s

    actual `shouldEqual` expected

tagsParserSpec ∷ Spec Unit
tagsParserSpec = describe "tagsParser" do
  it "parses a valid tags string" do
    let
      s = "tag1"
        <> "\n"
        <> "tag2"
        <> "\n"
    let
      expected =
        pure $ fromFoldable
          [ unsafeTag "tag1"
          , unsafeTag "tag2"
          ]
    let
      actual = runParser tagsParser s

    actual `shouldEqual` expected
