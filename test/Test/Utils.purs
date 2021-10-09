module Test.Utils
  ( appendNotes
  , createCommit
  , unsafeInstantFromSeconds
  , withGitRepo
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime, instant)
import Data.Either (Either(Left, Right))
import Data.Formatter.DateTime (format, parseFormatString)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.String (trim)
import Data.Time.Duration (Seconds(Seconds), convertDuration)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, bracket, error, throwError)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Git.Commit
  ( Author(Author)
  , CommitInfo(CommitInfo)
  , CommitRef
  , Committer(Committer)
  , Timestamp(Timestamp)
  , asHex
  , unsafeCommitMessage
  , unsafeCommitRef
  , unsafeEmail
  , unsafeTimezone
  , unsafeUser
  )
import Node.FS.Aff (mkdir, rmdir)
import Node.OS (tmpdir)
import Node.Path (FilePath, concat)
import Partial.Unsafe (unsafePartial)
import Shell (executeCommand)

type TestCommitInfo =
  { authorName ∷ String
  , committerName ∷ String
  , date ∷ DateTime
  , message ∷ String
  }

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

formatCommitDate ∷ DateTime → Either String String
formatCommitDate d = do
  dateFormat ← parseFormatString "ddd MMM D HH:mm:ss UTC YYYY"
  pure $ format dateFormat d

createCommit ∷ FilePath → TestCommitInfo → Aff (CommitRef /\ CommitInfo)
createCommit gitDirPath { authorName, committerName, date, message } =
  case formatCommitDate date of
    Left errMsg → throwError $ error errMsg
    Right dateString → do
      let
        authorEmail = authorName <> "@example.com"

        committerEmail = committerName <> "@example.com"

        timestamp = Timestamp $ fromDateTime date

        cmd = "GIT_AUTHOR_DATE='"
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
          <> "' git commit --allow-empty -m \""
          <> message
          <> "\""

        commitInfo = CommitInfo
          { author: Author
              { email: unsafeEmail authorEmail
              , timestamp
              , timezone: unsafeTimezone 0
              , user: unsafeUser authorName
              }
          , committer: Committer
              { email: unsafeEmail committerEmail
              , timestamp
              , timezone: unsafeTimezone 0
              , user: unsafeUser committerName
              }
          , message: unsafeCommitMessage message
          }

      void $ executeCommand gitDirPath cmd
      cmdOutput ← executeCommand gitDirPath "git rev-parse HEAD"

      let
        commitRef = unsafeCommitRef $ trim cmdOutput

      pure $ commitRef /\ commitInfo

appendNotes ∷ FilePath → CommitRef → String → Aff Unit
appendNotes gitDirPath commitRef message =
  let
    cmd = "git notes append -m '"
      <> message
      <> "' "
      <> (asHex commitRef)

  in
    void $ executeCommand gitDirPath cmd

unsafeInstantFromSeconds ∷ Int → Instant

unsafeInstantFromSeconds seconds =
  unsafePartial $ fromJust $ instant $ convertDuration $ Seconds $
    toNumber seconds
