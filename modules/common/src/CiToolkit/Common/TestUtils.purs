module Test.CiToolkit.Common.TestUtils
  ( appendNotes
  , createCommit
  , createTag
  , genCommitObject
  , genEmail
  , genTimestamp
  , genUsername
  , quickCheckGitObjectComponent
  , toResult
  , withGitRepo
  ) where

import Prelude

import CiToolkit.Common.Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , CommitLine
      ( MessageLine
      , UnknownLine
      , ParentLine
      , CommitterLine
      , AuthorLine
      )
  , CommitMessage(CommitMessage)
  , CommitParent(CommitParent)
  , Committer(Committer)
  , Notes(Notes)
  , Tree(Tree)
  , commitInfoParser
  , commitLinesParser
  , commitMessageParser
  , commitRefParser
  , commitRefsParser
  , notesParser
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeTreeRef
  )
import CiToolkit.Common.Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , CommitParent(CommitParent)
  , CommitRef
  , Committer(Committer)
  , Tree(Tree)
  , TreeRef
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeTreeRef
  )
import CiToolkit.Common.Git.Commit.UserInfo
  ( Email
  , Timestamp
  , UserInfo(UserInfo)
  , Username(Username)
  , emailParser
  , timestampParser
  , unsafeEmail
  , unsafeTimestamp
  , usernameParser
  )
import CiToolkit.Common.Git.Commit.UserInfo
  ( UserInfo(UserInfo)
  , Username(Username)
  , dateTimeToTimestamp
  , unsafeEmail
  , unsafeTimestampTimezone
  )
import CiToolkit.Common.Git.Object (class GitObjectComponent)
import CiToolkit.Common.Git.Object (showInGitObject)
import CiToolkit.Common.Shell (executeCommand)
import CiToolkit.Common.Utils
  ( unsafeCharFromCharCode
  , unsafeDateTimeFromSeconds
  , unsafeInstantFromSeconds
  , unsafeNonEmptyString
  )
import Data.Array.NonEmpty as NEA
import Data.Char (fromCharCode)
import Data.Char.Gen (genAsciiChar)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, instant, toDateTime)
import Data.Either (Either(Left, Right))
import Data.Formatter.DateTime (format, parseFormatString)
import Data.Int (toNumber)
import Data.List (List(Nil), fromFoldable)
import Data.List (List(Nil), singleton)
import Data.Maybe (Maybe, fromJust, maybe)
import Data.NonEmpty ((:|))
import Data.String (joinWith)
import Data.String (trim)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Gen (genString)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Time.Duration (Seconds(Seconds), convertDuration)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, bracket, error, throwError)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Node.FS.Aff (mkdir, rmdir)
import Node.OS (tmpdir)
import Node.Path (FilePath, concat)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (Result, (<?>))
import Test.QuickCheck (quickCheckGen')
import Test.QuickCheck.Gen
  ( Gen
  , arrayOf
  , arrayOf1
  , chooseInt
  , elements
  , suchThat
  , vectorOf
  )
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser (Parser, runParser)

type TestCommitInfo =
  { authorName ∷ String
  , committerName ∷ String
  , date ∷ DateTime
  , message ∷ String
  , parentRef ∷ Maybe CommitRef
  }

type TestCommitRefs =
  { commitRef ∷ CommitRef
  , treeRef ∷ TreeRef
  }

toResult
  ∷ ∀ a i
  . Eq a
  ⇒ Show a
  ⇒ Show i
  ⇒ i
  → { actual ∷ a, expected ∷ a }
  → Result
toResult info { actual, expected } =
  (actual == expected) <?>
    ( "Info: "
        <> show info
        <> "\nExpected: "
        <> show expected
        <> "\nActual: "
        <> show actual
    )

withGitRepo ∷ (FilePath → Aff Unit) → Aff Unit
withGitRepo = bracket initGitRepo cleanUpGitRepo

initGitRepo ∷ Aff FilePath
initGitRepo = do
  gitDirPath ← mkRandomDir
  void $ executeCommand gitDirPath "git init"
  pure gitDirPath

cleanUpGitRepo ∷ FilePath → Aff Unit
cleanUpGitRepo = rmdir

mkRandomDir ∷ Aff FilePath
mkRandomDir = do
  parentDirAbs ← liftEffect tmpdir
  idx ← liftEffect $ randomInt 100000000 999999999
  let dir = concat [ parentDirAbs, "git-repo-" <> show idx ]
  mkdir dir
  pure dir

createCommit
  ∷ FilePath → TestCommitInfo → Aff (TestCommitRefs /\ CommitInfo)
createCommit gitDirPath testCommitInfo = do
  cmd /\ commitInfo ← commitCommand
    testCommitInfo
    ( "commit --allow-empty -m \""
        <> testCommitInfo.message
        <> "\""
    )

  void $ executeCommand gitDirPath cmd

  commitRef ← (unsafeCommitRef <<< trim) <$> executeCommand
    gitDirPath
    "git rev-parse HEAD"

  treeRef ← (unsafeTreeRef <<< trim) <$> executeCommand
    gitDirPath
    "git rev-parse HEAD^{tree}"

  let (CommitInfo info) = commitInfo

  -- TODO handle treeRef better
  pure $ { commitRef, treeRef } /\
    (CommitInfo $ info { tree = Tree treeRef })

createTag
  ∷ FilePath → String → Aff Unit
createTag gitDirPath tagName = do
  void $ executeCommand gitDirPath ("git tag " <> tagName)

appendNotes ∷ FilePath → TestCommitInfo → CommitRef → Aff Unit
appendNotes gitDirPath testCommitInfo commitRef = do
  cmd /\ _ ← commitCommand
    testCommitInfo
    ( "notes append -m '"
        <> testCommitInfo.message
        <> "' "
        <> (showInGitObject commitRef)
    )

  void $ executeCommand gitDirPath cmd

commitCommand ∷ TestCommitInfo → String → Aff (String /\ CommitInfo)
commitCommand
  { authorName, committerName, date, message, parentRef }
  cmd =
  case formatCommitDate date of
    Left errMsg → throwError $ error errMsg
    Right dateString → do
      let
        authorEmail = authorName <> "@example.com"

        committerEmail = committerName <> "@example.com"

        timestamp = unsafePartial $ fromJust $
          dateTimeToTimestamp (unsafeTimestampTimezone 0) date

        commitInfo = CommitInfo
          { author: Author $ UserInfo
              { email: unsafeEmail authorEmail
              , timestamp
              , username: Username $ unsafePartial $ fromJust $
                  NES.fromString authorName
              }
          , committer: Committer $ UserInfo
              { email: unsafeEmail committerEmail
              , timestamp
              , username: Username $ unsafePartial $ fromJust $
                  NES.fromString committerName
              }
          , message: unsafeCommitMessage message
          , parents: maybe Nil (singleton <<< CommitParent) parentRef
          , tree: Tree $ unsafeTreeRef "dummy"
          }

        commandString = "GIT_AUTHOR_DATE='"
          <> dateString
          <> "' GIT_AUTHOR_NAME='"
          <> authorName
          <> "' GIT_AUTHOR_EMAIL='"
          <> authorEmail
          <> "' GIT_COMMITTER_DATE='"
          <> dateString
          <> "' GIT_COMMITTER_NAME='"
          <> committerName
          <> "' GIT_COMMITTER_EMAIL='"
          <> committerEmail
          <> "' git "
          <> cmd

      pure $ commandString /\ commitInfo

formatCommitDate ∷ DateTime → Either String String
formatCommitDate d = do
  dateFormat ← parseFormatString "ddd MMM D HH:mm:ss UTC YYYY"
  pure $ format dateFormat d

quickCheckGitObjectComponent
  ∷ ∀ a
  . Eq a
  ⇒ GitObjectComponent a
  ⇒ Show a
  ⇒ Gen a
  → Parser a
  → Aff Unit
quickCheckGitObjectComponent generate parser =
  liftEffect $ quickCheckGen' 200 $ do
    component ← generate

    let
      componentString = showInGitObject component
      actual = runParser parser componentString

    pure $ toResult
      { componentString }
      { actual, expected: pure component }

genCommitObject ∷ Gen CommitInfo
genCommitObject = ado
  author ← genAuthor
  committer ← genCommitter
  parents ← fromFoldable <$> arrayOf
    (CommitParent <$> genGitObjectRef unsafeCommitRef)
  tree ← (Tree <$> genGitObjectRef unsafeTreeRef)
  in
    CommitInfo
      { author
      , committer
      , message: CommitMessage "commit message"
      , parents
      , tree
      }

genAuthor ∷ Gen Author
genAuthor = ado
  userInfo ← genUserInfo
  in
    Author userInfo

genCommitter ∷ Gen Committer
genCommitter = ado
  userInfo ← genUserInfo
  in
    Committer userInfo

genUserInfo ∷ Gen UserInfo
genUserInfo = ado
  username ← genUsername
  email ← genEmail
  timestamp ← genTimestamp
  in
    UserInfo { username, email, timestamp }

genGitObjectRef ∷ ∀ a. (String → a) → Gen a
genGitObjectRef refFromString = ado
  chars ← vectorOf 40 $ elements $ NEA.fromNonEmpty $ '0' :|
    [ '1'
    , '2'
    , '3'
    , '4'
    , '5'
    , '6'
    , '7'
    , '8'
    , '9'
    , 'a'
    , 'b'
    , 'c'
    , 'd'
    , 'e'
    , 'f'
    ]
  in refFromString $ fromCharArray chars

genTimestamp ∷ Gen Timestamp
genTimestamp = do
  tz ← chooseInt 12 14
  ins ← chooseInt 0 2000000000
  pure $ unsafeTimestamp { ins, tz }

genDateTime ∷ Gen DateTime
genDateTime = pure $ unsafeDateTimeFromSeconds 1

genEmail ∷ Gen Email
genEmail = do
  localPartSegments ← arrayOf1 genUsernameSegment

  let
    localPart = NES.joinWith1
      (unsafeNonEmptyString ".")
      localPartSegments

  domain ← elements $ NEA.fromNonEmpty $
    "example1" :| [ "example2" ]

  tld ← elements $ NEA.fromNonEmpty $
    "com" :| [ "org" ]

  pure $ unsafeEmail $ joinWith
    "@"
    [ NES.toString localPart, domain <> "." <> tld ]

genUsername ∷ Gen Username
genUsername = do
  nameSegments ← arrayOf1 genUsernameSegment

  let s = NES.joinWith1 (unsafeNonEmptyString " ") nameSegments

  pure $ Username s

genUsernameSegment ∷ Gen String
genUsernameSegment = genString $ genAsciiChar `suchThat` \c →
  c /= ' ' && c /= '<' && c /= '>' && c /= '\n'
