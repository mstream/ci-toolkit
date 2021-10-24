module CiToolkit.Common.Git.Commit
  ( class GitObjectComponent
  , Author(..)
  , Committer(..)
  , CommitInfo(..)
  , CommitLine(..)
  , CommitMessage(..)
  , CommitParent(..)
  , CommitRef
  , Email
  , GitObjectRef
  , GitObjectRefFormat(..)
  , Notes(..)
  , Timestamp(..)
  , Timezone
  , Tree(..)
  , TreeRef
  , UserInfo(..)
  , Username(..)
  , commitInfoParser
  , commitLinesParser
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
  , unsafeTreeRef
  , unsafeEmail
  , unsafeTimezone
  ) where

import Prelude hiding (between)

import CiToolkit.Common.Git.Parsing
  ( eolParser
  , fullStringParser
  , linesParser
  , wordParser
  )
import CiToolkit.Common.Text.SerDe (class Serializable, serialize)
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

class GitObjectComponent a where
  gitObjectParser ∷ Parser a
  showInGitObject ∷ a → String

data GitObjectRefFormat
  = FullHex
  | ShortHex

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

newtype CommitParent = CommitParent CommitRef

derive instance Generic CommitParent _

instance Show CommitParent where
  show = genericShow

instance EncodeJson CommitParent where
  encodeJson = genericEncodeJson

instance Eq CommitParent where
  eq = genericEq

instance GitObjectComponent CommitParent where
  gitObjectParser = commitParentParser
  showInGitObject (CommitParent commitRef) =
    "parent " <> showInGitObject commitRef

newtype Tree = Tree TreeRef

derive instance Generic Tree _

instance Show Tree where
  show = genericShow

instance EncodeJson Tree where
  encodeJson = genericEncodeJson

instance Eq Tree where
  eq = genericEq

instance GitObjectComponent Tree where
  gitObjectParser = treeParser
  showInGitObject (Tree treeRef) =
    "tree " <> showInGitObject treeRef

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
    , parents ∷ List CommitParent
    , tree ∷ Tree
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
    showInGitObject info.tree
      <> "\n"
      <> joinWith
        "\n"
        (fromFoldable $ showInGitObject <$> info.parents)
      <> "\n"
      <> showInGitObject info.author
      <> "\n"
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

instance Serializable CommitMessage Unit where
  serialize _ (CommitMessage msgStr) = msgStr

newtype GitObjectRef =
  GitObjectRef String

derive instance Generic GitObjectRef _

instance Show GitObjectRef where
  show = genericShow

instance EncodeJson GitObjectRef where
  encodeJson = genericEncodeJson

instance Eq GitObjectRef where
  eq = genericEq

instance GitObjectComponent GitObjectRef where
  gitObjectParser = gitObjectRefParser
  showInGitObject = serialize FullHex

instance Serializable GitObjectRef GitObjectRefFormat where
  serialize format (GitObjectRef hexStr) = case format of
    FullHex → hexStr
    ShortHex → take 8 hexStr

newtype CommitRef =
  CommitRef GitObjectRef

derive instance Generic CommitRef _

instance Show CommitRef where
  show = genericShow

instance EncodeJson CommitRef where
  encodeJson = genericEncodeJson

instance Eq CommitRef where
  eq = genericEq

instance GitObjectComponent CommitRef where
  gitObjectParser = commitRefParser
  showInGitObject (CommitRef gitObjectRef) = serialize FullHex
    gitObjectRef

instance Serializable CommitRef GitObjectRefFormat where
  serialize format (CommitRef ref) = serialize format ref

newtype TreeRef =
  TreeRef GitObjectRef

derive instance Generic TreeRef _

instance Show TreeRef where
  show = genericShow

instance EncodeJson TreeRef where
  encodeJson = genericEncodeJson

instance Eq TreeRef where
  eq = genericEq

instance GitObjectComponent TreeRef where
  gitObjectParser = treeRefParser
  showInGitObject (TreeRef gitObjectRef) = serialize FullHex
    gitObjectRef

newtype Notes =
  Notes (List String)

derive instance Generic Notes _

instance Show Notes where
  show = genericShow

instance EncodeJson Notes where
  encodeJson = genericEncodeJson

instance Eq Notes where
  eq = genericEq

data CommitLine
  = AuthorLine Author
  | CommitterLine Committer
  | MessageLine CommitMessage
  | ParentLine CommitParent
  | TreeLine Tree
  | UnknownLine

derive instance Generic CommitLine _

instance Show CommitLine where
  show = genericShow

instance EncodeJson CommitLine where
  encodeJson = genericEncodeJson

instance Eq CommitLine where
  eq = genericEq

commitInfoParser ∷ Parser CommitInfo
commitInfoParser = do
  let
    r line acc = case line of
      AuthorLine author → acc { author = pure author }
      CommitterLine committer → acc { committer = pure committer }
      MessageLine (CommitMessage msgLine) → acc
        { messageLines = msgLine : acc.messageLines }
      ParentLine parent → acc { parents = parent : acc.parents }
      TreeLine tree → acc { tree = pure tree }
      UnknownLine → acc

  commitLines ← commitLinesParser

  let
    info = foldr
      r
      { author: Nothing
      , committer: Nothing
      , messageLines: Nil
      , parents: Nil
      , tree: Nothing
      }
      commitLines

  case info.author, info.committer, info.tree of
    Nothing, _, _ → fail "no author"
    _, Nothing, _ → fail "no committer"
    _, _, Nothing → fail "no tree"
    Just author, Just committer, Just tree → pure $ CommitInfo
      { author
      , committer
      , message: CommitMessage $ joinWith
          "\n"
          (fromFoldable $ reverse info.messageLines)
      , parents: info.parents
      , tree
      }

commitLinesParser ∷ Parser (List CommitLine)
commitLinesParser = commitLineParser `sepBy` eolParser

commitLineParser ∷ Parser CommitLine
commitLineParser = choice
  [ AuthorLine <$> authorParser
  , CommitterLine <$> committerParser
  , ParentLine <$> commitParentParser
  , TreeLine <$> treeParser
  , MessageLine <$> commitMessageParser
  , UnknownLine <$ (many $ noneOf [ '\n' ])
  ]

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

notesParser ∷ Parser Notes
notesParser = do
  lines ← linesParser
  pure $ Notes lines

commitParentParser ∷ Parser CommitParent
commitParentParser = do
  void $ string "parent"
  skipSpaces
  commitRef ← commitRefParser
  pure $ CommitParent commitRef

treeParser ∷ Parser Tree
treeParser = do
  void $ string "tree"
  skipSpaces
  treeRef ← treeRefParser
  pure $ Tree treeRef

commitMessageParser ∷ Parser CommitMessage
commitMessageParser = do
  chars ← many1 $ noneOf [ '\n' ]
  pure $ CommitMessage $ fromCharArray $ fromFoldable chars

commitRefParser ∷ Parser CommitRef
commitRefParser = CommitRef <$> gitObjectRefParser

treeRefParser ∷ Parser TreeRef
treeRefParser = TreeRef <$> gitObjectRefParser

commitRefsParser ∷ Parser (List CommitRef)
commitRefsParser = do
  refs ← commitRefParser `endBy` eolParser
  pure refs

gitObjectRefParser ∷ Parser GitObjectRef
gitObjectRefParser = do
  fullString ← fullStringParser
  if length fullString == 40 then pure $ GitObjectRef fullString
  else fail "not a 40-character long hex string"

unsafeCommitMessage ∷ String → CommitMessage
unsafeCommitMessage = CommitMessage

unsafeCommitRef ∷ String → CommitRef
unsafeCommitRef = CommitRef <<< GitObjectRef

unsafeTreeRef ∷ String → TreeRef
unsafeTreeRef = TreeRef <<< GitObjectRef

unsafeEmail ∷ String → Email
unsafeEmail = Email <<< unsafeNonEmptyString

unsafeTimezone ∷ Int → Timezone
unsafeTimezone = Timezone

unsafeNonEmptyString ∷ String → NonEmptyString
unsafeNonEmptyString s =
  unsafePartial $ fromJust $ NES.fromString s
