module CiToolkit.Common.Shell (executeCommand, executeCommand_) where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer (toString)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(UTF8))
import Node.Path (FilePath)

executeCommand ∷ FilePath → String → Aff String
executeCommand cwd cmd = liftEffect $ do
  outputBuffer ← execSync
    cmd
    (defaultExecSyncOptions { cwd = pure cwd })
  outputString ← toString UTF8 outputBuffer
  pure outputString

executeCommand_ ∷ String → Aff String
executeCommand_ = executeCommand "."
