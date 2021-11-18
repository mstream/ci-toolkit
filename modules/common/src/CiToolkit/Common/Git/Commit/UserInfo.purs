module CiToolkit.Common.Git.Commit.UserInfo
  ( Email
  , Timestamp(..)
  , TimestampInstant(..)
  , TimestampTimezone
  , UserInfo(..)
  , Username(..)
  , dateTimeToTimestamp
  , emailParser
  , timestampParser
  , timestampToDateTime
  , userInfoParser
  , usernameParser
  , unsafeEmail
  , unsafeTimestamp
  , unsafeTimestampInstant
  , unsafeTimestampTimezone
  ) where

import Prelude

import CiToolkit.Common.Git.Object
  ( class GitObjectComponent
  , gitObjectParser
  , showInGitObject
  )
import CiToolkit.Common.Git.Parsing
  ( eolParser
  , fullStringParser
  , linesParser
  , wordParser
  )
import CiToolkit.Common.Text.SerDe (class Serializable, serialize)
import CiToolkit.Common.Utils (unsafeNonEmptyString)
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

data GitObjectRefFormat
  = FullHex
  | ShortHex

newtype UserInfo =
  UserInfo
    { email ∷ Email
    , timestamp ∷ Timestamp
    , username ∷ Username
    }

derive instance Generic UserInfo _

instance Show UserInfo where
  show = genericShow

instance EncodeJson UserInfo where
  encodeJson
    ( UserInfo
        { email
        , timestamp
        , username
        }
    ) =
    "email" := encodeJson email
      ~> "timestamp" := encodeJson timestamp
      ~> "username" := encodeJson username
      ~> jsonEmptyObject

instance Eq UserInfo where
  eq = genericEq

instance GitObjectComponent UserInfo where
  gitObjectParser = userInfoParser
  showInGitObject (UserInfo { email, timestamp, username }) =
    joinWith
      " "
      [ showInGitObject username
      , "<" <> showInGitObject email <> ">"
      , showInGitObject timestamp
      ]

newtype Email =
  Email NonEmptyString

derive instance Generic Email _

instance Show Email where
  show = genericShow

instance EncodeJson Email where
  encodeJson = genericEncodeJson

instance Eq Email where
  eq = genericEq

instance GitObjectComponent Email where
  gitObjectParser = emailParser
  showInGitObject (Email s) = NES.toString s

data Timestamp = Timestamp
  { instant ∷ TimestampInstant
  , timezone ∷ TimestampTimezone
  }

derive instance Generic Timestamp _

instance Show Timestamp where
  show = genericShow

instance EncodeJson Timestamp where
  encodeJson (Timestamp { instant, timezone }) =
    "instant" := (encodeString $ show instant)
      ~> "timezone" := encodeJson timezone
      ~> jsonEmptyObject

instance Eq Timestamp where
  eq = genericEq

instance GitObjectComponent Timestamp where
  gitObjectParser = timestampParser
  showInGitObject (Timestamp { instant, timezone }) =
    showInGitObject instant
      <> " "
      <> showInGitObject timezone

newtype TimestampInstant =
  TimestampInstant Instant

derive instance Generic TimestampInstant _

instance Show TimestampInstant where
  show = genericShow

instance EncodeJson TimestampInstant where
  encodeJson (TimestampInstant ins) =
    let
      (Milliseconds millis) = unInstant $ ins

    in
      encodeNumber millis

instance Eq TimestampInstant where
  eq = genericEq

instance GitObjectComponent TimestampInstant where
  gitObjectParser = timestampInstantParser
  showInGitObject (TimestampInstant ins) =
    let
      (Milliseconds millis) = unInstant $ ins

    in
      toStringWith
        (fixed 0)
        (millis / 1000.0)

newtype TimestampTimezone =
  TimestampTimezone Int

derive instance Generic TimestampTimezone _

instance Show TimestampTimezone where
  show = genericShow

instance EncodeJson TimestampTimezone where
  encodeJson = genericEncodeJson

instance Eq TimestampTimezone where
  eq = genericEq

instance GitObjectComponent TimestampTimezone where
  gitObjectParser = timestampTimezoneParser
  showInGitObject (TimestampTimezone timezoneInt) =
    case timezoneInt of
      i
        | i < (-9) → "-" <> absValStr
        | i > 9 → "+" <> absValStr
        | i < 0 → "-0" <> absValStr
        | i > 0 → "+0" <> absValStr
        | i == 0 → "+0000"
        | otherwise → "???"
    where
    absValStr = Int.toStringAs
      Int.decimal
      (Int.round $ abs $ Int.toNumber $ timezoneInt * 100)

newtype Username =
  Username NonEmptyString

derive instance Generic Username _

instance Show Username where
  show = genericShow

instance EncodeJson Username where
  encodeJson = genericEncodeJson

instance Eq Username where
  eq = genericEq

instance GitObjectComponent Username where
  gitObjectParser = usernameParser
  showInGitObject (Username s) = NES.toString s

emailParser ∷ Parser Email
emailParser =
  do
    s ← regex "[^<>]+"
    maybe
      (fail "empty email")
      (pure <<< Email)
      (NES.fromString s)

usernameParser ∷ Parser Username
usernameParser = do
  s ← trim <$> regex "[^<>]+"
  maybe
    (fail "empty username")
    (pure <<< Username)
    (NES.fromString s)

timestampParser ∷ Parser Timestamp
timestampParser = do
  instant ← timestampInstantParser
  skipSpaces
  timezone ← timestampTimezoneParser
  pure $ Timestamp { instant, timezone }

timestampInstantParser ∷ Parser TimestampInstant
timestampInstantParser = do
  timestampString ← wordParser

  let
    timestamp = do
      timestampInt ← Int.fromString $ NES.toString timestampString

      TimestampInstant <$>
        ( instant $ convertDuration $ Seconds $ Int.toNumber
            timestampInt
        )

  maybe
    (fail "unparsable timestamp")
    pure
    timestamp

timestampTimezoneParser ∷ Parser TimestampTimezone
timestampTimezoneParser = do
  timezoneString ← regex "[+-]\\d{4}"
  maybe
    (fail "unparsable timezone")
    (pure <<< TimestampTimezone <<< (_ / 100))
    (Int.fromString timezoneString)

timestampToDateTime ∷ Timestamp → Maybe DateTime
timestampToDateTime (Timestamp { instant, timezone }) =
  let
    (TimestampInstant ins) = instant
    (TimestampTimezone tz) = timezone
  in
    adjust (Hours $ Int.toNumber tz) (toDateTime ins)

dateTimeToTimestamp ∷ TimestampTimezone → DateTime → Maybe Timestamp
dateTimeToTimestamp (TimestampTimezone tz) dateTime = do
  ins ← fromDateTime <$> adjust (Hours $ Int.toNumber (-tz)) dateTime
  pure $ Timestamp
    { instant: TimestampInstant ins
    , timezone: TimestampTimezone tz
    }

userInfoParser ∷ Parser UserInfo
userInfoParser = do
  let
    emailStart = string "<"
    emailEnd = string ">"

  username ← gitObjectParser
  void $ emailStart
  email ← gitObjectParser
  void $ emailEnd
  skipSpaces
  timestamp ← gitObjectParser
  pure $ UserInfo { email, timestamp, username }

unsafeEmail ∷ String → Email
unsafeEmail = Email <<< unsafeNonEmptyString

unsafeTimestamp ∷ { ins ∷ Int, tz ∷ Int } → Timestamp
unsafeTimestamp { ins, tz } =
  Timestamp
    { instant: unsafeTimestampInstant ins
    , timezone: unsafeTimestampTimezone tz
    }

unsafeTimestampInstant ∷ Int → TimestampInstant
unsafeTimestampInstant ins =
  TimestampInstant
    $ unsafePartial
    $ fromJust
    $ instant
    $ convertDuration
    $ Seconds
    $ Int.toNumber
    $ ins

unsafeTimestampTimezone ∷ Int → TimestampTimezone
unsafeTimestampTimezone = TimestampTimezone
