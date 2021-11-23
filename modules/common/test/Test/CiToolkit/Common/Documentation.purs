module Test.CiToolkit.Common.Documentation (spec) where

import Prelude

import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandInfo(CommandInfo)
  , CommandOption(CommandOption)
  , HowTo(HowTo)
  , commandInfoToAsciiDoc
  )
import Data.Maybe (Maybe(Nothing))
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
        { description:
            [ "commandDescriptionLine1"
            , "commandDescriptionLine2"
            ]
        , howTos:
            [ HowTo
                { codeSnippets:
                    [ CodeSnippet
                        { code:
                            [ "codeSnippetCode1Line1"
                            , "codeSnippetCode1Line2"
                            ]
                        , title: pure "codeSnippetTitle1"
                        }
                    ]
                , title: "howToTitle1"
                }
            , HowTo
                { codeSnippets:
                    [ CodeSnippet
                        { code: [ "codeSnippetCode2" ]
                        , title: Nothing
                        }
                    ]
                , title: "howToTitle2"
                }
            ]
        , name: "commandName"
        , options:
            [ CommandOption
                { description:
                    [ "optionDescription1Line1"
                    , "optionDescription1Line2"
                    ]
                , longForm: "option1"
                , shortForm: pure "o1"
                }
            , CommandOption
                { description: [ "optionDescription2" ]
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
        , "commandDescriptionLine1"
        , "commandDescriptionLine2"
        , ""
        , "== How to..."
        , ""
        , "=== howToTitle1"
        , ""
        , ".codeSnippetTitle1"
        , "[source,bash]"
        , "----"
        , "codeSnippetCode1Line1"
        , "codeSnippetCode1Line2"
        , "----"
        , ""
        , "=== howToTitle2"
        , ""
        , "[source,bash]"
        , "----"
        , "codeSnippetCode2"
        , "----"
        , ""
        , "== Reference"
        , ""
        , "=== option1"
        , ""
        , "optionDescription1Line1"
        , "optionDescription1Line2"
        , ""
        , "=== option2"
        , ""
        , "optionDescription2"
        , ""
        ]
      actual = commandInfoToAsciiDoc commandInfo

    actual `shouldEqual` expected
