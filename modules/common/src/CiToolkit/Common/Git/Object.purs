module CiToolkit.Common.Git.Object
  ( class GitObjectComponent
  , GitObjectRef
  , GitObjectRefFormat(..)
  , gitObjectParser
  , showInGitObject
  ) where

import Prelude

import CiToolkit.Common.Git.Parsing
  ( eolParser
  , fullStringParser
  , linesParser
  , wordParser
  )
import CiToolkit.Common.Text.SerDe (class Serializable, serialize)
import Control.Alt ((<|>))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Argonaut.Encode.Encoders
  ( encodeInt
  , encodeNumber
  , encodeString
  )
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (fromFoldable)
import Data.Date (Date)
import Data.Date as Date
import Data.DateTime (DateTime(DateTime), adjust)
import Data.DateTime.Instant
  ( Instant
  , fromDateTime
  , instant
  , toDateTime
  , unInstant
  )
import Data.Either
  ( Either(Left, Right)
  , either
  , hush
  , note
  )
import Data.Either.Nested (type (\/))
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldMap, foldr, oneOf)
import Data.Formatter.DateTime (FormatterCommand(UnixTimestamp), format)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List(Nil), filter, last, reverse, singleton, (:))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Number.Format (fixed, toStringWith)
import Data.Ord (class Ord)
import Data.Ord.Generic (genericCompare)
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
import Data.Time (Time(Time))
import Data.Time as Time
import Data.Time.Duration
  ( Hours(Hours)
  , Milliseconds(Milliseconds)
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

class GitObjectComponent a where
  gitObjectParser ∷ Parser a
  showInGitObject ∷ a → String

data GitObjectRefFormat
  = FullHex
  | ShortHex

newtype GitObjectRef =
  GitObjectRef String

derive instance Generic GitObjectRef _

instance Show GitObjectRef where
  show = genericShow

instance EncodeJson GitObjectRef where
  encodeJson = genericEncodeJson

instance Eq GitObjectRef where
  eq = genericEq

instance Ord GitObjectRef where
  compare = genericCompare

instance GitObjectComponent GitObjectRef where
  gitObjectParser = gitObjectRefParser
  showInGitObject = serialize FullHex

instance Serializable GitObjectRef GitObjectRefFormat where
  serialize format (GitObjectRef hexStr) = case format of
    FullHex → hexStr
    ShortHex → take 8 hexStr

gitObjectRefParser ∷ Parser GitObjectRef
gitObjectRefParser = do
  fullString ← fullStringParser
  if length fullString == 40 then pure $ GitObjectRef fullString
  else fail "not a 40-character long hex string"
