module Test.CiToolkit.Common.Documentation (spec) where

import Prelude

import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandInfo(CommandInfo)
  , CommandOptionDescription(CommandOptionDescription)
  , HowTo(HowTo)
  , codeSnippetToAsciiDoc
  , commandInfoToAsciiDoc
  , commandOptionDescriptionToAsciiDoc
  , howToToAsciiDoc
  )
import Data.Maybe (Maybe)
import Data.String (joinWith)
import Options.Applicative as Opts
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Documentation" do
  codeSnippetToAsciiDocSpec
  commandInfoToAsciiDocSpec
  commandOptionDescriptionToAsciiDocSpec
  commandSuiteInfoToAsciiDocSpec
  howToToAsciiDocSpec

codeSnippetToAsciiDocSpec ∷ Spec Unit
codeSnippetToAsciiDocSpec = describe "codeSnippetToAsciiDoc" do
  it "converts correctly" do
    let
      codeSnippet = CodeSnippet
        { code:
            [ "codeSnippetCodeLine1"
            , "codeSnippetCodeLine2"
            ]
        , title: pure "codeSnippetTitle"
        }
      expected = joinWith
        "\n"
        [ ".codeSnippetTitle"
        , "[source,bash]"
        , "----"
        , "codeSnippetCodeLine1"
        , "codeSnippetCodeLine2"
        , "----"
        ]
      actual = codeSnippetToAsciiDoc codeSnippet

    actual `shouldEqual` expected

commandInfoToAsciiDocSpec ∷ Spec Unit
commandInfoToAsciiDocSpec = describe "commandInfoToAsciiDoc" do
  it "converts correctly" do
    let
      commandInfo = CommandInfo
        { description: [ "commandDescription" ]
        , howTos:
            [ HowTo
                { codeSnippets:
                    [ CodeSnippet
                        { code: [ "codeSnippetCode1" ]
                        , title: pure "codeSnippetTitle1"
                        }
                    ]
                , title: "howToTitle1"
                }
            , HowTo
                { codeSnippets:
                    [ CodeSnippet
                        { code: [ "codeSnippetCode2" ]
                        , title: pure "codeSnippetTitle2"
                        }
                    ]
                , title: "howToTitle2"
                }
            ]
        , name: "commandName"
        , options:
            [ CommandOptionDescription
                { defaultValue: pure "'optionDefaultValue1'"
                , description: [ "optionDescription1" ]
                , longForm: "option1"
                , shortForm: pure 'a'
                }
            , CommandOptionDescription
                { defaultValue: pure "'optionDefaultValue2'"
                , description: [ "optionDescription2" ]
                , longForm: "option2"
                , shortForm: pure 'b'
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
        , ".codeSnippetTitle1"
        , "[source,bash]"
        , "----"
        , "codeSnippetCode1"
        , "----"
        , ""
        , "=== howToTitle2"
        , ""
        , ".codeSnippetTitle2"
        , "[source,bash]"
        , "----"
        , "codeSnippetCode2"
        , "----"
        , ""
        , "== Reference"
        , ""
        , "=== option1"
        , ""
        , "optionDescription1"
        , ""
        , "Default value: *'optionDefaultValue1'*"
        , ""
        , "=== option2"
        , ""
        , "optionDescription2"
        , ""
        , "Default value: *'optionDefaultValue2'*"
        ]
      actual = commandInfoToAsciiDoc commandInfo

    actual `shouldEqual` expected

commandOptionDescriptionToAsciiDocSpec ∷ Spec Unit
commandOptionDescriptionToAsciiDocSpec = describe
  "commandOptionDescriptionToAsciiDoc"
  do
    it "converts correctly option with a default value" do
      let
        commandOptionDescription = CommandOptionDescription
          { defaultValue: pure "'optionDefaultValue'"
          , description:
              [ "optionDescriptionLine1"
              , "optionDescriptionLine2"
              ]
          , longForm: "option"
          , shortForm: pure 'o'
          }
        expected = joinWith
          "\n"
          [ "=== option"
          , ""
          , "optionDescriptionLine1"
          , "optionDescriptionLine2"
          , ""
          , "Default value: *'optionDefaultValue'*"
          ]
        actual = commandOptionDescriptionToAsciiDoc
          commandOptionDescription

      actual `shouldEqual` expected

commandSuiteInfoToAsciiDocSpec ∷ Spec Unit
commandSuiteInfoToAsciiDocSpec = describe "commandSuiteInfoToAsciiDoc"
  do
    it "converts correctly" do
      -- TODO
      true `shouldEqual` true

howToToAsciiDocSpec ∷ Spec Unit
howToToAsciiDocSpec = describe "howToToAsciiDoc" do
  it "converts correctly" do
    let
      howTo = HowTo
        { codeSnippets:
            [ CodeSnippet
                { code:
                    [ "codeSnippet1CodeLine1"
                    , "codeSnippet1CodeLine2"
                    ]
                , title: pure "codeSnippetTitle1"
                }
            , CodeSnippet
                { code:
                    [ "codeSnippet2CodeLine1"
                    , "codeSnippet2CodeLine2"
                    ]
                , title: pure "codeSnippetTitle2"
                }
            ]
        , title: "howToTitle"
        }

      expected = joinWith
        "\n"
        [ "=== howToTitle"
        , ""
        , ".codeSnippetTitle1"
        , "[source,bash]"
        , "----"
        , "codeSnippet1CodeLine1"
        , "codeSnippet1CodeLine2"
        , "----"
        , ""
        , ".codeSnippetTitle2"
        , "[source,bash]"
        , "----"
        , "codeSnippet2CodeLine1"
        , "codeSnippet2CodeLine2"
        , "----"
        ]

      actual = howToToAsciiDoc howTo

    actual `shouldEqual` expected
