module CiToolkit.Common.Documentation.Version.Show
  ( ShowOptions(..)
  , VersionFormat(..)
  , commandInfo
  , showOptionsParser
  ) where

import Prelude

import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandInfo(CommandInfo)
  , CommandOption(CommandOption)
  , HowTo(HowTo)
  , commandOptionDescription
  , commandOptionToParser
  , gitDirectoryOption
  )
import CiToolkit.Common.Version
  ( VersionTagPrefix(VersionTagPrefix)
  , versionTagPrefixParser
  )
import Data.Either (Either(Left, Right), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Options.Applicative as Opts
import Text.Parsing.StringParser (runParser)
import Type.Proxy (Proxy(Proxy))

data Command
  = Show ShowOptions
  | Version

derive instance Generic Command _

instance Show Command where
  show = genericShow

newtype ShowOptions = ShowOptions
  { format ∷ VersionFormat
  , versionTagPrefix ∷ VersionTagPrefix
  }

derive instance Generic ShowOptions _

instance Show ShowOptions where
  show = genericShow

showOptionsParser ∷ Opts.Parser ShowOptions
showOptionsParser = ado
  format ← commandOptionToParser versionFormatOption
  versionTagPrefix ← commandOptionToParser versionTagPrefixOption
  in
    ShowOptions { format, versionTagPrefix }

data VersionFormat
  = Calendar
  | Semantic

instance Show VersionFormat where
  show = case _ of
    Calendar → "calendar"
    Semantic → "semantic"

commandInfo ∷ CommandInfo
commandInfo = CommandInfo
  { description: [ "Calculate a version of the current commit." ]
  , howTos:
      [ HowTo
          { codeSnippets:
              [ CodeSnippet
                  { code: [ "git tag $(npx @ci-toolkit/version show)" ]
                  , title: Nothing
                  }
              ]
          , title: "create a release tag"
          }
      ]
  , name: "show"
  , options:
      [ commandOptionDescription versionFormatOption
      , commandOptionDescription gitDirectoryOption
      , commandOptionDescription versionTagPrefixOption
      ]
  }

versionFormatOption ∷ CommandOption VersionFormat
versionFormatOption = CommandOption
  { defaultValue: pure Semantic
  , description: [ "Format of the version" ]
  , longForm: "format"
  , reader: Opts.eitherReader $ case _ of
      "calendar" → pure Calendar
      "semantic" → pure Semantic
      unsupportedFormat → Left $ "unsupported format: " <>
        unsupportedFormat
  , shortForm: Nothing
  }

versionTagPrefixOption ∷ CommandOption VersionTagPrefix
versionTagPrefixOption = CommandOption
  { defaultValue: pure $
      VersionTagPrefix (nes (Proxy ∷ Proxy "v"))
  , description: [ "Prefix for version-related Git tags" ]
  , longForm: "version-prefix"
  , reader: Opts.eitherReader $ \s →
      case runParser versionTagPrefixParser s of
        Left { error } → Left error
        Right versionTagPrefix → pure versionTagPrefix
  , shortForm: Nothing
  }
