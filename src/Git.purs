module Git (getCommitInfo, getCommitNotes, getCommitRefs) where

import Prelude

import Data.Either (hush)
import Data.List (List(Nil), filter, fromFoldable)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Effect (Effect)
import Effect.Exception (catchException, throw)
import Git.Commit
  ( CommitInfo
  , CommitRef
  , Notes
  , asHex
  , commitInfoParser
  , commitRefsParser
  , notesParser
  )
import Node.Buffer (toString)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(UTF8))
import Text.Parsing.StringParser (runParser)

getCommitRefs ∷ Effect (List CommitRef)
getCommitRefs = do
  cmdOutput ← executeShellCommand "git rev-list --all"
  maybe
    (throw "cannot parse commit refs")
    pure
    (hush $ runParser commitRefsParser cmdOutput)

getCommitNotes ∷ CommitRef → Effect (Maybe Notes)
getCommitNotes commitRef =
  catchException (\_ → pure Nothing) do
    cmdOutput ← executeShellCommand $
      "git notes show " <> asHex commitRef
    pure $ hush $ runParser notesParser cmdOutput

getCommitInfo ∷ CommitRef → Effect (Maybe CommitInfo)
getCommitInfo commitRef = do
  cmdOutput ← executeShellCommand
    $ "git cat-file -p " <> asHex commitRef
  pure $ hush $ runParser commitInfoParser cmdOutput

executeShellCommand ∷ String → Effect String
executeShellCommand cmd = do
  outputBuffer ← execSync cmd defaultExecSyncOptions
  outputString ← toString UTF8 outputBuffer
  pure outputString
