module Git
  ( appendCommitNotes
  , getCommitInfo
  , getCommitNotes
  , getCommitRefs
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
  , asHex
  , commitInfoParser
  , commitRefsParser
  , notesParser
  )
import Node.Path (FilePath)
import Shell (executeCommand)
import Text.Parsing.StringParser (runParser)

getCommitRefs ∷ FilePath → Aff (List CommitRef)
getCommitRefs gitDirPath = do
  cmdOutput ← executeCommand
    gitDirPath
    "git rev-list --all --invert-grep --grep 'Notes added by'"
  maybe
    (throwError $ error "cannot parse commit refs")
    pure
    (hush $ runParser commitRefsParser cmdOutput)

appendCommitNotes ∷ FilePath → CommitRef → Notes → Aff Unit
appendCommitNotes gitDirPath commitRef (Notes notes) = do
  let
    cmd = "git notes append -m '"
      <> (joinWith "\n" (fromFoldable notes))
      <> "' "
      <> asHex
        commitRef
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
      ("git notes show " <> asHex commitRef)
    pure $ hush $ runParser notesParser cmdOutput

getCommitInfo ∷ FilePath → CommitRef → Aff CommitInfo
getCommitInfo gitDirPath commitRef = do
  cmdOutput ← executeCommand
    gitDirPath
    ("git cat-file -p " <> asHex commitRef)
  either
    (throwError <<< error <<< (_.error))
    pure
    (runParser commitInfoParser cmdOutput)
