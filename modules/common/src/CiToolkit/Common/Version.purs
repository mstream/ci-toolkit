module CiToolkit.Common.Version
  ( VersionTagPrefix(..)
  , unsafeVersionTagPrefix
  , versionTagPrefixParser
  ) where

import Prelude

import CiToolkit.Common.Git.Parsing (wordParser)
import CiToolkit.Common.Utils (unsafeNonEmptyString)
import Data.Eq.Generic
  ( genericEq
  )
import Data.Generic.Rep
  ( class Generic
  )
import Data.Show.Generic
  ( genericShow
  )
import Data.String.NonEmpty (NonEmptyString)
import Text.Parsing.StringParser (Parser)

newtype VersionTagPrefix = VersionTagPrefix NonEmptyString

derive instance Generic VersionTagPrefix _

instance Show VersionTagPrefix where
  show = genericShow

instance Eq VersionTagPrefix where
  eq = genericEq

versionTagPrefixParser ∷ Parser VersionTagPrefix
versionTagPrefixParser = do
  versionTagPrefixName ← wordParser
  pure $ VersionTagPrefix versionTagPrefixName

unsafeVersionTagPrefix ∷ String → VersionTagPrefix
unsafeVersionTagPrefix = VersionTagPrefix <<< unsafeNonEmptyString
