module Git.Commit
  ( class GitObjectComponent
  , Author(..)
  , Committer(..)
  , CommitInfo(..)
  , CommitMessage(..)
  , CommitRef
  , Email
  , Notes(..)
  , Timestamp(..)
  , Timezone
  , UserInfo(..)
  , Username(..)
  , asHex
  , commitInfoParser
  , commitMessageParser
  , commitRefParser
  , commitRefsParser
  , emailParser
  , gitObjectParser
  , notesParser
  , showInGitObject
  , timestampParser
  , timezoneParser
  , usernameParser
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimezone
  ) where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Encoders (encodeInt, encodeNumber)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (fromFoldable)
import Data.Array.NonEmpty (fromFoldable1)
import Data.DateTime.Instant (Instant, fromDateTime, instant, unInstant)
import Data.Eq.Generic (genericEq)
import Data.Int as Int
import Data.Either
  ( Either(Left, Right)
  , either
  , hush
  , note
  )
import Data.Either.Nested (type (\/))
import Data.Foldable (oneOf, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust, maybe)
import Data.Number.Format (fixed, toStringWith)
import Data.List (List(Nil), filter)
import Data.List.NonEmpty as NEL
import Data.Time.Duration
  ( Milliseconds(Milliseconds)
  , Seconds(Seconds)
  , convertDuration
  )
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern), joinWith, length, split, trim)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.CodeUnits (fromNonEmptyCharArray)
import Math (abs)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser)
import Text.Parsing.StringParser.CodePoints
  ( anyChar
  , eof
  , regex
  , skipSpaces
  , string
  )
import Text.Parsing.StringParser.Combinators
  ( between
  , endBy
  , endBy1
  , many
  , many1Till
  , manyTill
  , sepBy
  )

class GitObjectComponent a where
  gitObjectParser ∷ Parser a
  showInGitObject ∷ a → String

newtype Author =
  Author UserInfo

derive instance Generic Author _

instance Show Author where
  show = genericShow

instance EncodeJson Author where
  encodeJson = genericEncodeJson

instance Eq Author where
  eq = genericEq

instance GitObjectComponent Author where
  gitObjectParser = authorParser
  showInGitObject (Author userInfo) =
    "author " <> showInGitObject userInfo

newtype Committer =
  Committer UserInfo

derive instance Generic Committer _

instance Show Committer where
  show = genericShow

instance EncodeJson Committer where
  encodeJson = genericEncodeJson

instance Eq Committer where
  eq = genericEq

instance GitObjectComponent Committer where
  gitObjectParser = committerParser
  showInGitObject (Committer userInfo) =
    "committer " <> showInGitObject userInfo

newtype UserInfo =
  UserInfo
    { email ∷ Email
    , timestamp ∷ Timestamp
    , timezone ∷ Timezone
    , username ∷ Username
    }

derive instance Generic UserInfo _

instance Show UserInfo where
  show = genericShow

instance EncodeJson UserInfo where
  encodeJson = genericEncodeJson

instance Eq UserInfo where
  eq = genericEq

instance GitObjectComponent UserInfo where
  gitObjectParser = userInfoParser
  showInGitObject (UserInfo { email, timestamp, timezone, username }) =
    joinWith
      " "
      [ showInGitObject username
      , "<" <> showInGitObject email <> ">"
      , showInGitObject timestamp
      , showInGitObject timezone
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

newtype Timestamp =
  Timestamp Instant

derive instance Generic Timestamp _

instance Show Timestamp where
  show = genericShow

instance EncodeJson Timestamp where
  encodeJson (Timestamp ins) =
    let
      (Milliseconds millis) = unInstant ins

    in
      encodeNumber millis

instance Eq Timestamp where
  eq = genericEq

instance GitObjectComponent Timestamp where
  gitObjectParser = timestampParser
  showInGitObject (Timestamp ins) =
    let
      (Milliseconds millis) = unInstant ins

    in
      toStringWith
        (fixed 0)
        (millis / 1000.0)

newtype Timezone =
  Timezone Int

derive instance Generic Timezone _

instance Show Timezone where
  show = genericShow

instance EncodeJson Timezone where
  encodeJson = genericEncodeJson

instance Eq Timezone where
  eq = genericEq

instance GitObjectComponent Timezone where
  gitObjectParser = timezoneParser
  showInGitObject (Timezone timezoneInt) =
    case timezoneInt of
      i
        | i < (-9) → "-" <> absValStr
        | i > 9 → "+" <> absValStr
        | i < 0 → "-0" <> absValStr
        | i > 0 → "+0" <> absValStr
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

newtype CommitInfo =
  CommitInfo
    { author ∷ Author
    , committer ∷ Committer
    , message ∷ CommitMessage
    }

derive instance Generic CommitInfo _

instance Show CommitInfo where
  show = genericShow

instance EncodeJson CommitInfo where
  encodeJson = genericEncodeJson

instance Eq CommitInfo where
  eq = genericEq

instance GitObjectComponent CommitInfo where
  gitObjectParser = commitInfoParser
  showInGitObject (CommitInfo info) =
    showInGitObject info.author <> "\n"
      <> showInGitObject info.committer
      <> "\n\n"
      <> showInGitObject info.message

newtype CommitMessage =
  CommitMessage String

derive instance Generic CommitMessage _

instance Show CommitMessage where
  show = genericShow

instance EncodeJson CommitMessage where
  encodeJson = genericEncodeJson

instance Eq CommitMessage where
  eq = genericEq

instance GitObjectComponent CommitMessage where
  gitObjectParser = commitMessageParser
  showInGitObject (CommitMessage msg) = msg

newtype CommitRef =
  CommitRef String

derive instance Generic CommitRef _

instance Show CommitRef where
  show = genericShow

instance EncodeJson CommitRef where
  encodeJson = genericEncodeJson

instance Eq CommitRef where
  eq = genericEq

newtype Notes =
  Notes (List String)

derive instance Generic Notes _

instance Show Notes where
  show = genericShow

instance EncodeJson Notes where
  encodeJson = genericEncodeJson

instance Eq Notes where
  eq = genericEq

newLineParser ∷ Parser Unit
newLineParser = void $ string "\n"

fullStringParser ∷ Parser String
fullStringParser = regex ".*"

linesParser ∷ Parser (List String)
linesParser = fullStringParser `sepBy` newLineParser

wordParser ∷ Parser NonEmptyString
wordParser = do
  word ← regex "[^ ]+"
  maybe
    (fail "empty word")
    pure
    (NES.fromString word)

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
  timestampString ← wordParser

  let
    timestamp = do
      timestampInt ← Int.fromString $ NES.toString timestampString

      let
        duration = Seconds $ Int.toNumber timestampInt

      Timestamp <$> (instant $ convertDuration duration)

  maybe
    (fail "unparsable timestamp")
    pure
    timestamp

timezoneParser ∷ Parser Timezone
timezoneParser = do
  timezoneString ← regex "[+-]\\d{4}"
  maybe (fail "unparsable timezone")
    (pure <<< Timezone <<< (_ / 100))
    (Int.fromString timezoneString)

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
  skipSpaces
  timezone ← gitObjectParser
  pure $ UserInfo
    { email
    , timestamp
    , timezone
    , username
    }

authorParser ∷ Parser Author
authorParser = do
  void $ string "author"
  skipSpaces
  userInfo ← gitObjectParser
  pure $ Author userInfo

committerParser ∷ Parser Committer
committerParser = do
  void $ string "committer"
  skipSpaces
  userInfo ← gitObjectParser
  pure $ Committer userInfo

commitInfoParser ∷ Parser CommitInfo
commitInfoParser = do
  lines ← linesParser
  let
    result = ado
      author ← note
        "no valid author line"
        (oneOf $ hush <<< runParser authorParser <$> lines)

      committer ← note "no valid committer line"
        (oneOf $ hush <<< runParser committerParser <$> lines)

      message ← note
        "no commit message"
        ( hush $ runParser commitMessageParser
            (joinWith "\n" (fromFoldable lines))
        )

      in { author, committer, message }
  case result of
    Left errMsg → fail errMsg
    Right { author, committer, message } → pure $ CommitInfo
      { author, committer, message }

notesParser ∷ Parser Notes
notesParser = do
  lines ← linesParser
  pure $ Notes lines

commitMessageParser ∷ Parser CommitMessage
commitMessageParser = do
  void $ manyTill (manyTill anyChar newLineParser) newLineParser
  messageString ← fullStringParser
  pure $ CommitMessage messageString

commitRefParser ∷ Parser CommitRef
commitRefParser = do
  fullString ← fullStringParser
  if length fullString == 40 then pure $ CommitRef fullString
  else fail "not a 40-character long hex string"

commitRefsParser ∷ Parser (List CommitRef)
commitRefsParser = do
  refs ← commitRefParser `endBy` newLineParser
  pure refs

asHex ∷ CommitRef → String
asHex (CommitRef commitRefString) = commitRefString

unsafeCommitMessage ∷ String → CommitMessage
unsafeCommitMessage = CommitMessage

unsafeCommitRef ∷ String → CommitRef
unsafeCommitRef = CommitRef

unsafeEmail ∷ String → Email
unsafeEmail = Email <<< unsafeNonEmptyString

unsafeTimezone ∷ Int → Timezone
unsafeTimezone = Timezone

unsafeNonEmptyString ∷ String → NonEmptyString
unsafeNonEmptyString s =
  unsafePartial $ fromJust $ NES.fromString s
