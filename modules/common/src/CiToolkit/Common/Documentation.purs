module CiToolkit.Common.Documentation
  ( CodeSnippet(..)
  , CommandInfo(..)
  , CommandOption(..)
  , CommandOptionDescription(..)
  , CommandSuiteInfo(..)
  , HowTo(..)
  , ProjectInfo(..)
  , ciStagePrefixOption
  , codeSnippetToAsciiDoc
  , commandInfoToAsciiDoc
  , commandOptionDescription
  , commandOptionDescriptionToAsciiDoc
  , commandOptionToParser
  , commandSuiteInfoToAsciiDoc
  , dryRunOption
  , gitDirectoryOption
  , howToToAsciiDoc
  , outputFormatOption
  , projectInfoToAsciiDoc
  , verboseOption
  ) where

import Prelude

import CiToolkit.Common.AsciiDoc as ADoc
import CiToolkit.Common.CI (CIStagePrefix(CIStagePrefix))
import CiToolkit.Common.ProgramInput (parseCIPrefix, parseOutputFormat)
import CiToolkit.Common.ProgramOutcome (OutputFormat(JSON))
import Control.Plus (empty)
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.String (joinWith)
import Data.String.NonEmpty (nes)
import Options.Applicative as Opts
import Options.Applicative.Common (mapParser, treeMapParser)
import Options.Applicative.Help.Chunk (extractChunk)
import Options.Applicative.Types
  ( OptHelpInfo
  , OptProperties(OptProperties)
  , OptTree(AltNode, Leaf, MultNode)
  , Option(Option)
  )
import Text.PrettyPrint.Leijen (renderPretty)
import Type.Proxy (Proxy(Proxy))

newtype ProjectInfo = ProjectInfo
  { commandSuites ∷ Array CommandSuiteInfo
  , name ∷ String
  }

newtype CommandSuiteInfo = CommandSuiteInfo
  { commands ∷ Array CommandInfo
  , description ∷ Array String
  , howTos ∷ Array HowTo
  , name ∷ String
  }

newtype CommandInfo = CommandInfo
  { description ∷ Array String
  , howTos ∷ Array HowTo
  , name ∷ String
  , options ∷ Array CommandOptionDescription
  }

newtype HowTo = HowTo
  { codeSnippets ∷ Array CodeSnippet
  , title ∷ String
  }

newtype CodeSnippet = CodeSnippet
  { code ∷ Array String, title ∷ Maybe String }

newtype CommandOptionDescription = CommandOptionDescription
  { defaultValue ∷ Maybe String
  , description ∷ Array String
  , longForm ∷ String
  , shortForm ∷ Maybe Char
  }

newtype CommandOption a = CommandOption
  { defaultValue ∷ Maybe a
  , description ∷ Array String
  , longForm ∷ String
  , reader ∷ Opts.ReadM a
  , shortForm ∷ Maybe Char
  }

commandOptionDescription
  ∷ ∀ a. Show a ⇒ CommandOption a → CommandOptionDescription

commandOptionDescription
  (CommandOption { defaultValue, description, longForm, shortForm }) =
  CommandOptionDescription
    { defaultValue: show <$> defaultValue
    , description
    , longForm
    , shortForm
    }

commandOptionToParser ∷ ∀ a. CommandOption a → Opts.Parser a
commandOptionToParser
  ( CommandOption
      { defaultValue, description, longForm, reader, shortForm }
  ) =
  Opts.option
    reader
    ( (maybe mempty Opts.value defaultValue)
        <> (Opts.help $ joinWith "\n" description)
        <> (Opts.long longForm)
        <> (maybe mempty Opts.short shortForm)
    )

ciStagePrefixOption ∷ CommandOption CIStagePrefix
ciStagePrefixOption = CommandOption
  { defaultValue: pure $ CIStagePrefix (nes (Proxy ∷ Proxy "ci-"))
  , description:
      [ "Prefix for stage names to differentiate the from other Git notes entries."
      ]
  , longForm: "ci-prefix"
  , reader: parseCIPrefix
  , shortForm: empty
  }

dryRunOption ∷ CommandOption Boolean
dryRunOption = CommandOption
  { defaultValue: pure false
  , description:
      [ "When enabled, a simulated outcome of the command is displayed"
      , "but no change to the Git repository is made."
      ]
  , longForm: "dry-run"
  , reader: Opts.boolean
  , shortForm: empty
  }

gitDirectoryOption ∷ CommandOption String
gitDirectoryOption = CommandOption
  { defaultValue: pure "."
  , description: [ "Git repository path" ]
  , longForm: "git-directory"
  , reader: Opts.str
  , shortForm: Nothing
  }

outputFormatOption ∷ CommandOption OutputFormat
outputFormatOption = CommandOption
  { defaultValue: pure JSON
  , description: [ "Format that the output should be produced with" ]
  , longForm: "format"
  , reader: parseOutputFormat
  , shortForm: empty
  }

verboseOption ∷ CommandOption Boolean
verboseOption = CommandOption
  { defaultValue: pure false
  , description:
      [ "Output the maximum amount of diagnostic information." ]
  , longForm: "verbose"
  , reader: Opts.boolean
  , shortForm: pure 'v'
  }

projectInfoToAsciiDoc ∷ ProjectInfo → String
projectInfoToAsciiDoc (ProjectInfo { commandSuites, name }) =
  let
    lines =
      [ ADoc.title ADoc.L0 name
      , ""
      , ADoc.title ADoc.L1 "Modules"
      , ""
      ] <> (ADoc.bullet <<< commandSuiteInfoToLink <$> commandSuites)
  in
    joinWith "\n" lines

commandSuiteInfoToAsciiDoc ∷ CommandSuiteInfo → String
commandSuiteInfoToAsciiDoc
  (CommandSuiteInfo { commands, description, howTos, name }) =
  let
    lines =
      [ ADoc.title ADoc.L0 name
      , ""
      , ADoc.title ADoc.L1 "Description"
      , ""
      , joinWith "\n" description
      , ""
      , ADoc.title ADoc.L1 "How to..."
      , ""
      ]
        <>
          (howToToAsciiDoc <$> howTos)
        <>
          [ ADoc.title ADoc.L1 "Commands"
          , ""
          ]
        <> (ADoc.bullet <<< commandInfoToLink <$> commands)
  in
    joinWith
      "\n"
      lines

commandInfoToLink ∷ CommandInfo → String
commandInfoToLink (CommandInfo { name }) = ADoc.link
  { alias: name, href: name <> ".adoc" }

commandSuiteInfoToLink ∷ CommandSuiteInfo → String
commandSuiteInfoToLink (CommandSuiteInfo { name }) = ADoc.link
  { alias: name, href: "modules/" <> name <> "/README.adoc" }

commandInfoToAsciiDoc ∷ CommandInfo → String
commandInfoToAsciiDoc
  ( CommandInfo
      { description
      , howTos
      , name
      , options
      }
  ) =
  let
    lines =
      [ ADoc.title ADoc.L0 name
      , ""
      , ADoc.title ADoc.L1 "Description"
      , ""
      , joinWith "\n" description
      , ""
      , ADoc.title ADoc.L1 "How to..."
      ]
        <>
          (append "\n" <<< howToToAsciiDoc <$> howTos)
        <>
          [ "", ADoc.title ADoc.L1 "Reference" ]
        <>
          ( append "\n" <<< commandOptionDescriptionToAsciiDoc <$>
              options
          )
  in
    joinWith "\n" lines

commandOptionDescriptionToAsciiDoc ∷ CommandOptionDescription → String
commandOptionDescriptionToAsciiDoc
  ( CommandOptionDescription
      { defaultValue, description, longForm, shortForm }
  ) =
  let
    lines =
      [ ADoc.title ADoc.L2 longForm
      , ""
      , joinWith "\n" description
      ]
        <>
          ( maybe
              []
              (\v → [ "", "Default value: *" <> v <> "*" ])
              defaultValue
          )

  in
    joinWith
      "\n"
      lines

howToToAsciiDoc ∷ HowTo → String
howToToAsciiDoc (HowTo { codeSnippets, title }) =
  let
    lines =
      [ ADoc.title ADoc.L2 title
      ]
        <> (append "\n" <<< codeSnippetToAsciiDoc <$> codeSnippets)
  in
    joinWith "\n" lines

codeSnippetToAsciiDoc ∷ CodeSnippet → String
codeSnippetToAsciiDoc (CodeSnippet { code, title }) =
  ADoc.sourceBlock { code: joinWith "\n" code, language: "bash", title }
