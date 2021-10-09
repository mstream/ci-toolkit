module Git.Commit
  ( Author(..)
  , Committer(..)
  , CommitInfo(..)
  , CommitMessage
  , CommitRef
  , Email
  , Notes(..)
  , Timestamp(..)
  , Timezone
  , User
  , asHex
  , commitInfoParser
  , commitMessageParser
  , commitRefParser
  , commitRefsParser
  , notesParser
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimezone
  , unsafeUser
  ) where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Encoders (encodeInt)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (fromFoldable)
import Data.DateTime.Instant (Instant, fromDateTime, instant, unInstant)
import Data.Eq.Generic (genericEq)
import Data.Int (fromString, round, toNumber)
import Data.Either
  ( Either(Left, Right)
  , either
  , hush
  , note
  )
import Data.Foldable (oneOf, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.List (List(Nil), filter)
import Data.Time.Duration
  ( Milliseconds(Milliseconds)
  , Seconds(Seconds)
  , convertDuration
  )
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern), joinWith, length, split)
import Text.Parsing.StringParser (Parser, fail, runParser)
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
  , many
  , manyTill
  , sepBy
  )

newtype Author =
  Author
    { email ∷ Email
    , timestamp ∷ Timestamp
    , timezone ∷ Timezone
    , user ∷ User
    }

derive instance Generic Author _

instance Show Author where
  show = genericShow

instance EncodeJson Author where
  encodeJson = genericEncodeJson

instance Eq Author where
  eq = genericEq

newtype Committer =
  Committer
    { email ∷ Email
    , timestamp ∷ Timestamp
    , timezone ∷ Timezone
    , user ∷ User
    }

derive instance Generic Committer _

instance Show Committer where
  show = genericShow

instance EncodeJson Committer where
  encodeJson = genericEncodeJson

instance Eq Committer where
  eq = genericEq

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

newtype CommitMessage =
  CommitMessage String

derive instance Generic CommitMessage _

instance Show CommitMessage where
  show = genericShow

instance EncodeJson CommitMessage where
  encodeJson = genericEncodeJson

instance Eq CommitMessage where
  eq = genericEq

newtype CommitRef =
  CommitRef String

derive instance Generic CommitRef _

instance Show CommitRef where
  show = genericShow

instance EncodeJson CommitRef where
  encodeJson = genericEncodeJson

instance Eq CommitRef where
  eq = genericEq

newtype Email =
  Email String

derive instance Generic Email _

instance Show Email where
  show = genericShow

instance EncodeJson Email where
  encodeJson = genericEncodeJson

instance Eq Email where
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
      encodeInt $ round millis

instance Eq Timestamp where
  eq = genericEq

newtype Timezone =
  Timezone Int

derive instance Generic Timezone _

instance Show Timezone where
  show = genericShow

instance EncodeJson Timezone where
  encodeJson = genericEncodeJson

instance Eq Timezone where
  eq = genericEq

newtype User =
  User String

derive instance Generic User _

instance Show User where
  show = genericShow

instance EncodeJson User where
  encodeJson = genericEncodeJson

instance Eq User where
  eq = genericEq

newLineParser ∷ Parser Unit
newLineParser = void $ string "\n"

fullStringParser ∷ Parser String
fullStringParser = regex ".*"

linesParser ∷ Parser (List String)
linesParser = fullStringParser `sepBy` newLineParser

wordParser ∷ Parser String
wordParser = regex "[^ ]+"

emailParser ∷ Parser Email
emailParser =
  do
    void $ string "<"
    emailString ← regex "[^>]+"
    void $ string ">"
    pure $ Email emailString

userParser ∷ Parser User
userParser = do
  userString ← wordParser
  pure $ User userString

timestampParser ∷ Parser Timestamp
timestampParser = do
  timestampString ← wordParser

  let
    timestamp = do
      duration ← Seconds <$> (toNumber <$> fromString timestampString)
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
    (fromString timezoneString)

authorParser ∷ Parser Author
authorParser = do
  void $ string "author "
  user ← userParser
  void $ string " "
  email ← emailParser
  void $ string " "
  timestamp ← timestampParser
  void $ string " "
  timezone ← timezoneParser
  pure $ Author { email, timestamp, timezone, user }

committerParser ∷ Parser Committer
committerParser = do
  void $ string "committer "
  user ← userParser
  skipSpaces
  email ← emailParser
  skipSpaces
  timestamp ← timestampParser
  skipSpaces
  timezone ← timezoneParser
  pure $ Committer { email, timestamp, timezone, user }

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
unsafeEmail = Email

unsafeTimezone ∷ Int → Timezone
unsafeTimezone = Timezone

unsafeUser ∷ String → User
unsafeUser = User
