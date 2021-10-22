module Test.Git.Tag (spec) where

import Prelude

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
import Git.Tag (tagParser, tagsParser, unsafeTag)
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
import Test.Utils
  ( toResult
  , unsafeInstantFromSeconds
  , unsafeNonEmptyString
  )
import Text.Parsing.StringParser (Parser, runParser)
import Type.Proxy (Proxy(Proxy))

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
