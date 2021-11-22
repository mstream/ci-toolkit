module Test.CiToolkit.Common.Documentation (spec) where

import Prelude

import CiToolkit.Common.Documentation
  ( CommandInfo(CommandInfo)
  , CommandOption(CommandOption)
  , HowTo(HowTo)
  , commandInfoToAsciiDoc
  )
import Data.String (joinWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Documentation" do
  commandInfoToAsciiDocSpec

commandInfoToAsciiDocSpec ∷ Spec Unit
commandInfoToAsciiDocSpec = describe "commandInfoToAsciiDoc" do
  it "converts correctly" do
    let
      commandInfo = CommandInfo
        { description: "commandDescription"
        , howTos:
            [ HowTo
                { codeSnippet: "codeSnippet1"
                , title: "howToTitle1"
                }
            , HowTo
                { codeSnippet: "codeSnippet2"
                , title: "howToTitle2"
                }
            ]
        , name: "commandName"
        , options:
            [ CommandOption
                { description: "optionDescription1"
                , longForm: "option1"
                , shortForm: pure "o1"
                }
            , CommandOption
                { description: "optionDescription2"
                , longForm: "option2"
                , shortForm: pure "o2"
                }
            ]
        }
      expected = joinWith
        "\n"
        [ "= commandName"
        , ""
        , "== Description"
        , ""
        , "commandDescription"
        , ""
        , "== How to..."
        , ""
        , "=== howToTitle1"
        , ""
        , "[source,bash]"
        , "----"
        , "codeSnippet1"
        , "----"
        , ""
        , "=== howToTitle2"
        , ""
        , "[source,bash]"
        , "----"
        , "codeSnippet2"
        , "----"
        , ""
        , "== Reference"
        , ""
        , "=== option1"
        , ""
        , "optionDescription1"
        , ""
        , "=== option2"
        , ""
        , "optionDescription2"
        , ""
        ]
      actual = commandInfoToAsciiDoc commandInfo

    actual `shouldEqual` expected
