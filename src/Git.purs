module Git (getCommitInfo, getCommitNotes, getCommitRefs) where

import Prelude

import Data.Either (hush)
import Data.List (List)
import Data.Maybe (Maybe(Nothing), maybe)
import Effect (Effect)
import Effect.Aff (Aff, catchError, throwError)
import Effect.Exception (error)
import Git.Commit
  ( CommitInfo
  , CommitRef
  , Notes
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
  cmdOutput ← executeCommand gitDirPath "git rev-list --all"
  maybe
    (throwError $ error "cannot parse commit refs")
    pure
    (hush $ runParser commitRefsParser cmdOutput)

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

getCommitInfo ∷ FilePath → CommitRef → Aff (Maybe CommitInfo)
getCommitInfo gitDirPath commitRef = do
  cmdOutput ← executeCommand
    gitDirPath
    ("git cat-file -p " <> asHex commitRef)
  pure $ hush $ runParser commitInfoParser cmdOutput
