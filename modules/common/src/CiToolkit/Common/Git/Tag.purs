module CiToolkit.Common.Git.Tag
  ( Tag
  , TagInfo
  , getTagCommitRef
  , tagInfoParser
  , tagParser
  , tagsParser
  , unsafeTag
  ) where

import Prelude hiding (between)

import CiToolkit.Common.Git.Commit (CommitRef, commitRefParser)
import CiToolkit.Common.Git.Parsing
  ( eolParser
  , fullStringParser
  , wordParser
  )
import CiToolkit.Common.Text.SerDe (class Serializable, serialize)
import CiToolkit.Common.Utils (unsafeNonEmptyString)
import Control.Alt ((<|>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Encoders (encodeInt, encodeNumber)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (fromFoldable)
import Data.DateTime.Instant (Instant, fromDateTime, instant, unInstant)
import Data.Either
  ( Either(Left, Right)
  , either
  , hush
  , note
  )
import Data.Either.Nested (type (\/))
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldMap, foldr, oneOf)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List(Nil), filter, last, reverse, singleton, (:))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Number.Format (fixed, toStringWith)
import Data.Show.Generic (genericShow)
import Data.String
  ( Pattern(Pattern)
  , joinWith
  , length
  , split
  , take
  , trim
  )
import Data.String.CodeUnits (fromCharArray)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.CodeUnits (fromNonEmptyCharArray)
import Data.Time.Duration
  ( Milliseconds(Milliseconds)
  , Seconds(Seconds)
  , convertDuration
  )
import Math (abs)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser
  ( ParseError
  , Parser
  , fail
  , runParser
  )
import Text.Parsing.StringParser.CodePoints
  ( anyChar
  , eof
  , noneOf
  , regex
  , skipSpaces
  , string
  )
import Text.Parsing.StringParser.Combinators
  ( between
  , choice
  , endBy
  , endBy1
  , many
  , many1
  , many1Till
  , manyTill
  , optional
  , sepBy
  )

newtype Tag = Tag NonEmptyString

derive instance Generic Tag _

instance Show Tag where
  show = genericShow

instance EncodeJson Tag where
  encodeJson = genericEncodeJson

instance Eq Tag where
  eq = genericEq

instance Serializable Tag Unit where
  serialize _ (Tag name) = NES.toString name

newtype TagInfo =
  TagInfo
    { commitRef ∷ CommitRef }

derive instance Generic TagInfo _

instance Show TagInfo where
  show = genericShow

instance EncodeJson TagInfo where
  encodeJson = genericEncodeJson

instance Eq TagInfo where
  eq = genericEq

tagsParser ∷ Parser (List Tag)
tagsParser = do
  tags ← tagParser `endBy` eolParser
  pure tags

tagParser ∷ Parser Tag
tagParser =
  Tag <$> wordParser

tagInfoParser ∷ Parser TagInfo
tagInfoParser = do
  commitRef ← commitRefParser
  pure $ TagInfo { commitRef }

getTagCommitRef ∷ TagInfo → CommitRef
getTagCommitRef (TagInfo { commitRef }) = commitRef

unsafeTag ∷ String → Tag
unsafeTag = Tag <<< unsafeNonEmptyString
