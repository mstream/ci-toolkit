module Test.Utils (createCommit, withGitRepo) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, bracket, catchError, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Random (randomInt)
import Node.FS.Aff (mkdir, rmdir)
import Node.OS (tmpdir)
import Node.Path (FilePath, concat)
import Shell (executeCommand)
import Test.Spec (Spec, after, before)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Effect.Console (log)

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

createCommit ∷ FilePath → String → Aff Unit
createCommit gitDirPath message = do
  let
    date = "Sun Oct  3 14:19:11 BST 2021"
    cmd = "GIT_AUTHOR_DATE='"
      <> date
      <> "' GIT_COMMITTER_DATE='"
      <> date
      <>
        "' git commit --allow-empty -m \"commit1\""
  void $ executeCommand gitDirPath cmd
