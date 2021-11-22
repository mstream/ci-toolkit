module CiToolkit.Common.Documentation
  ( CommandInfo(..)
  , CommandOption(..)
  , CommandSuiteInfo(..)
  , HowTo(..)
  , ProjectInfo(..)
  , commandInfoToAsciiDoc
  , commandSuiteInfoToAsciiDoc
  , gitDirectoryOption
  , projectInfoToAsciiDoc
  ) where

import Prelude

import CiToolkit.Common.AsciiDoc as ADoc
import Data.Maybe (Maybe(Nothing))
import Data.String (joinWith)

newtype ProjectInfo = ProjectInfo
  { commandSuites ∷ Array CommandSuiteInfo
  , name ∷ String
  }

newtype CommandSuiteInfo = CommandSuiteInfo
  { commands ∷ Array CommandInfo
  , description ∷ String
  , name ∷ String
  }

newtype CommandInfo = CommandInfo
  { description ∷ String
  , howTos ∷ Array HowTo
  , name ∷ String
  , options ∷ Array CommandOption
  }

newtype HowTo = HowTo
  { codeSnippet ∷ String, title ∷ String }

newtype CommandOption = CommandOption
  { description ∷ String
  , longForm ∷ String
  , shortForm ∷ Maybe String
  }

gitDirectoryOption ∷ CommandOption
gitDirectoryOption = CommandOption
  { description: "Git repository path"
  , longForm: "git-directory"
  , shortForm: Nothing
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
  (CommandSuiteInfo { commands, description, name }) =
  let
    lines =
      [ ADoc.title ADoc.L0 name
      , ""
      , ADoc.title ADoc.L1 "Description"
      , ""
      , description
      , ""
      , ADoc.title ADoc.L1 "Commands"
      , ""
      ] <> (ADoc.bullet <<< commandInfoToLink <$> commands)
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
  (CommandInfo { description, howTos, name, options }) =
  let
    lines =
      [ ADoc.title ADoc.L0 name
      , ""
      , ADoc.title ADoc.L1 "Description"
      , ""
      , description
      , ""
      , ADoc.title ADoc.L1 "How to..."
      , ""
      ]
        <>
          (howToToAsciiDoc <$> howTos)
        <>
          [ ADoc.title ADoc.L1 "Reference", "" ]
        <>
          (commandOptionToAsciiDoc <$> options)
  in
    joinWith "\n" lines

commandOptionToAsciiDoc ∷ CommandOption → String
commandOptionToAsciiDoc
  (CommandOption { description, longForm, shortForm }) =
  joinWith
    "\n"
    [ ADoc.title ADoc.L2 longForm
    , ""
    , description
    , ""
    ]

howToToAsciiDoc ∷ HowTo → String
howToToAsciiDoc
  (HowTo { codeSnippet, title }) =
  joinWith
    "\n"
    [ ADoc.title ADoc.L2 title
    , ""
    , ADoc.sourceBlock { code: codeSnippet, language: "bash" }
    , ""
    ]
