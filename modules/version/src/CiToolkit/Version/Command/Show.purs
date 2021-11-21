module CiToolkit.Version.Command.Show
  ( ShowOptions(..)
  , VersionFormat(..)
  , showCommandDescription
  , showOptionsParser
  ) where

import Prelude

import CiToolkit.Common.Version
  ( VersionTagPrefix(VersionTagPrefix)
  , versionTagPrefixParser
  )
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Options.Applicative as Opts
import Text.Parsing.StringParser (runParser)
import Type.Proxy (Proxy(Proxy))

data VersionFormat
  = Calendar
  | Semantic

derive instance Generic VersionFormat _

instance Show VersionFormat where
  show = genericShow

data Command = Show ShowOptions | Version

derive instance Generic Command _

instance Show Command where
  show = genericShow

newtype ShowOptions = ShowOptions
  { format ∷ VersionFormat, versionTagPrefix ∷ VersionTagPrefix }

derive instance Generic ShowOptions _

instance Show ShowOptions where
  show = genericShow

showOptionsParser ∷ Opts.Parser ShowOptions
showOptionsParser = ado
  format ← versionFormatParser
  versionTagPrefix ← Opts.option parseVersionPrefix
    ( Opts.long "version-prefix" <>
        (Opts.value $ VersionTagPrefix (nes (Proxy ∷ Proxy "v")))
    )
  in
    ShowOptions { format, versionTagPrefix }

versionFormatParser ∷ Opts.Parser VersionFormat
versionFormatParser =
  Opts.option parseVersionFormat
    (Opts.long "format" <> Opts.value Semantic)

parseVersionFormat ∷ Opts.ReadM VersionFormat
parseVersionFormat = Opts.eitherReader $ \s →
  case s of
    "calendar" → pure Calendar
    "semantic" → pure Semantic
    _ → Left "unsupported format"

parseVersionPrefix ∷ Opts.ReadM VersionTagPrefix
parseVersionPrefix = Opts.eitherReader $ \s →
  case runParser versionTagPrefixParser s of
    Left { error } → Left error
    Right versionPrefix → pure versionPrefix

showCommandDescription ∷ String
showCommandDescription = "Calculate a version of the current commit."
