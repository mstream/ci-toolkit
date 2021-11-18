module CiToolkit.Common.Git.Commit
  ( Author(..)
  , Committer(..)
  , CommitInfo(..)
  , CommitLine(..)
  , CommitMessage(..)
  , CommitParent(..)
  , CommitRef
  , GitObjectRef
  , GitObjectRefFormat(..)
  , Notes(..)
  , Tree(..)
  , TreeRef
  , commitDateTime
  , commitInfoParser
  , commitLinesParser
  , commitMessageParser
  , commitRefParser
  , commitRefsParser
  , notesParser
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeTreeRef
  ) where

import Prelude hiding (between)

import CiToolkit.Common.Git.Commit.UserInfo
  ( Timestamp(Timestamp)
  , TimestampInstant(TimestampInstant)
  , UserInfo(UserInfo)
  , timestampToDateTime
  , userInfoParser
  )
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
import Data.Time.Duration (Hours)
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

instance Ord GitObjectRef where
  compare = genericCompare

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

instance Ord CommitRef where
  compare = genericCompare

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

authorParser ∷ Parser Author
authorParser = do
  void $ string "author "
  userInfo ← userInfoParser
  pure $ Author userInfo

committerParser ∷ Parser Committer
committerParser = do
  void $ string "committer"
  userInfo ← userInfoParser
  pure $ Committer userInfo

gitObjectRefParser ∷ Parser GitObjectRef
gitObjectRefParser = do
  fullString ← fullStringParser
  if length fullString == 40 then pure $ GitObjectRef fullString
  else fail "not a 40-character long hex string"

commitDateTime ∷ CommitInfo → Maybe DateTime
commitDateTime (CommitInfo { committer }) =
  let
    Committer (UserInfo { timestamp }) = committer

  in
    timestampToDateTime timestamp

unsafeCommitMessage ∷ String → CommitMessage
unsafeCommitMessage = CommitMessage

unsafeCommitRef ∷ String → CommitRef
unsafeCommitRef = CommitRef <<< GitObjectRef

unsafeTreeRef ∷ String → TreeRef
unsafeTreeRef = TreeRef <<< GitObjectRef
