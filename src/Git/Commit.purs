module Git.Commit
  ( Author(..)
  , Committer(..)
  , CommitInfo(..)
  , CommitRef
  , Email
  , Notes(..)
  , Timestamp
  , Timezone
  , User
  , asHex
  , commitInfoParser
  , commitRefParser
  , commitRefsParser
  , notesParser
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimestamp
  , unsafeTimezone
  , unsafeUser
  ) where

import Prelude

import Data.Argonaut.Encode
  ( class EncodeJson
  )

import Data.Argonaut.Encode.Generic
  ( genericEncodeJson
  )

import Data.Eq.Generic
  ( genericEq
  )

import Data.Int
  ( fromString
  )

import Data.Either
  ( Either(Left, Right)
  , either
  , hush
  , note
  )

import Data.Foldable
  ( oneOf
  )

import Data.Generic.Rep
  ( class Generic
  )

import Data.Maybe
  ( maybe
  )

import Data.List
  ( List(Nil)
  , filter
  , fromFoldable
  )

import Data.Show.Generic
  ( genericShow
  )

import Data.String
  ( Pattern(Pattern)
  , length
  , split
  )

import Text.Parsing.StringParser
  ( Parser
  , fail
  , runParser
  )

import Text.Parsing.StringParser.CodePoints
  ( anyChar
  , eof
  , regex
  , string
  , string
  )

import Text.Parsing.StringParser.Combinators
  ( endBy
  , many
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
    }

derive instance Generic CommitInfo _

instance Show CommitInfo where
  show = genericShow

instance EncodeJson CommitInfo where
  encodeJson = genericEncodeJson

instance Eq CommitInfo where
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
  Timestamp Int

derive instance Generic Timestamp _

instance Show Timestamp where
  show = genericShow

instance EncodeJson Timestamp where
  encodeJson = genericEncodeJson

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

emailParser ∷ Parser Email
emailParser =
  do
    void $ string "<"
    emailString ← regex "[^>]+"
    void $ string ">"
    pure $ Email emailString

userParser ∷ Parser User
userParser = do
  userString ← regex "[^ ]+"
  pure $ User userString

timestampParser ∷ Parser Timestamp
timestampParser = do
  timestampString ← regex "[^ ]+"
  maybe
    (fail "unparsable timestamp")
    (pure <<< Timestamp)
    (fromString timestampString)

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
  void $ string " "
  email ← emailParser
  void $ string " "
  timestamp ← timestampParser
  void $ string " "
  timezone ← timezoneParser
  pure $ Committer { email, timestamp, timezone, user }

commitInfoParser ∷ Parser CommitInfo
commitInfoParser = do
  lines ← regex ".*" `sepBy` (string "\n")
  let
    result = ado
      author ← note
        "no valid author line"
        (oneOf $ hush <<< runParser authorParser <$> lines)

      committer ← note "no valid committer line"
        (oneOf $ hush <<< runParser committerParser <$> lines)

      in { author, committer }
  case result of
    Left errMsg → fail errMsg
    Right { author, committer } → pure $ CommitInfo
      { author, committer }

notesParser ∷ Parser Notes
notesParser = do
  lines ← regex ".*" `sepBy` (string "\n")
  pure $ Notes lines

commitRefParser ∷ Parser CommitRef
commitRefParser = do
  fullString ← regex ".*"
  if length fullString == 40 then pure $ CommitRef fullString
  else fail "not a 40-character long hex string"

commitRefsParser ∷ Parser (List CommitRef)
commitRefsParser = do
  refs ← commitRefParser `endBy` (string "\n")
  void $ eof
  pure refs

asHex ∷ CommitRef → String
asHex (CommitRef commitRefString) = commitRefString

unsafeCommitRef ∷ String → CommitRef
unsafeCommitRef = CommitRef

unsafeEmail ∷ String → Email
unsafeEmail = Email

unsafeTimestamp ∷ Int → Timestamp
unsafeTimestamp = Timestamp

unsafeTimezone ∷ Int → Timezone
unsafeTimezone = Timezone

unsafeUser ∷ String → User
unsafeUser = User
