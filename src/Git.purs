module Git
  ( appendCommitNotes
  , getCommitInfo
  , getCommitNotes
  , getCommitRefs
  , getTagInfo
  , getTags
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Either (either, hush)
import Data.List (List)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.String (joinWith)
import Effect.Aff (Aff, catchError, throwError)
import Effect.Exception (error)
import Git.Commit
  ( CommitInfo
  , CommitRef
  , Notes(..)
  , commitInfoParser
  , commitRefParser
  , commitRefsParser
  , notesParser
  , showInGitObject
  )
import Git.Tag (Tag, TagInfo, tagInfoParser, tagsParser)
import Node.Path (FilePath)
import Shell (executeCommand)
import Text.Parsing.StringParser (runParser)
import Text.SerDe (serialize)

getCommitRefs ∷ FilePath → Aff (List CommitRef)
getCommitRefs gitDirPath = do
  cmdOutput ← executeCommand
    gitDirPath
    "git rev-list --all --invert-grep --grep 'Notes added by'"
  maybe
    (throwError $ error "cannot parse commit refs")
    pure
    (hush $ runParser commitRefsParser cmdOutput)

getTags ∷ FilePath → Aff (List Tag)
getTags gitDirPath = do
  cmdOutput ← executeCommand
    gitDirPath
    "git tag"
  maybe
    (throwError $ error "cannot parse tags")
    pure
    (hush $ runParser tagsParser cmdOutput)

appendCommitNotes ∷ FilePath → CommitRef → Notes → Aff Unit
appendCommitNotes gitDirPath commitRef (Notes notes) = do
  let
    cmd = "git notes append -m '"
      <> (joinWith "\n" (fromFoldable notes))
      <> "' "
      <> showInGitObject commitRef
  void $ executeCommand
    gitDirPath
    cmd

getCommitNotes ∷ FilePath → CommitRef → Aff (Maybe Notes)
getCommitNotes gitDirPath commitRef =
  catchError
    run
    (\_ → pure Nothing)
  where
  run ∷ Aff (Maybe Notes)
  run = do
    cmdOutput ← executeCommand
      gitDirPath
      ("git notes show " <> showInGitObject commitRef)
    pure $ hush $ runParser notesParser cmdOutput

getCommitInfo ∷ FilePath → CommitRef → Aff CommitInfo
getCommitInfo gitDirPath commitRef = do
  cmdOutput ← executeCommand
    gitDirPath
    ("git cat-file -p " <> showInGitObject commitRef)
  either
    (throwError <<< error <<< (_.error))
    pure
    (runParser commitInfoParser cmdOutput)

getTagInfo ∷ FilePath → Tag → Aff TagInfo
getTagInfo gitDirPath tag = do
  cmdOutput ← executeCommand
    gitDirPath
    ("git rev-parse " <> serialize unit tag)
  either
    (throwError <<< error <<< (_.error))
    pure
    (runParser tagInfoParser cmdOutput)
