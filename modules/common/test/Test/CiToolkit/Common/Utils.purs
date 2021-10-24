module Test.CiToolkit.Common.Utils
  ( appendNotes
  , createCommit
  , createTag
  , toResult
  , unsafeCharFromCharCode
  , unsafeInstantFromSeconds
  , unsafeNonEmptyString
  , withGitRepo
  ) where

import Prelude

import CiToolkit.Common.Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , CommitParent(CommitParent)
  , CommitRef
  , Committer(Committer)
  , Timestamp(Timestamp)
  , Tree(Tree)
  , TreeRef
  , UserInfo(UserInfo)
  , Username(Username)
  , showInGitObject
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimezone
  , unsafeTreeRef
  )
import CiToolkit.Common.Shell (executeCommand)
import Data.Char (fromCharCode)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime, instant)
import Data.Either (Either(Left, Right))
import Data.Formatter.DateTime (format, parseFormatString)
import Data.Int (toNumber)
import Data.List (List(Nil), singleton)
import Data.Maybe (Maybe, fromJust, maybe)
import Data.String (trim)
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

        timestamp = Timestamp $ fromDateTime date

        commitInfo = CommitInfo
          { author: Author $ UserInfo
              { email: unsafeEmail authorEmail
              , timestamp
              , timezone: unsafeTimezone 0
              , username: Username $ unsafePartial $ fromJust $
                  NES.fromString authorName
              }
          , committer: Committer $ UserInfo
              { email: unsafeEmail committerEmail
              , timestamp
              , timezone: unsafeTimezone 0
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

unsafeCharFromCharCode ∷ Int → Char
unsafeCharFromCharCode i =
  unsafePartial $ fromJust $ fromCharCode i

unsafeNonEmptyString ∷ String → NonEmptyString
unsafeNonEmptyString s =
  unsafePartial $ fromJust $ NES.fromString s

unsafeInstantFromSeconds ∷ Int → Instant
unsafeInstantFromSeconds seconds =
  unsafePartial $ fromJust $ instant $ convertDuration $ Seconds $
    toNumber seconds
