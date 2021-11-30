module CiToolkit.Pipeline.Program (execute) where

import Prelude

import CiToolkit.Common.CI
  ( CIStage(CIStage)
  , CIStagePrefix(CIStagePrefix)
  , loadRepo
  )
import CiToolkit.Common.CLI (executeVersion)
import CiToolkit.Common.Documentation.Pipeline.GetLast
  ( GetLastOptions(GetLastOptions)
  )
import CiToolkit.Common.Documentation.Pipeline.MarkCommit
  ( MarkCommitOptions(MarkCommitOptions)
  )
import CiToolkit.Common.Git (appendCommitNotes)
import CiToolkit.Common.Git.Commit
  ( GitObjectRefFormat(FullHex)
  , Notes(Notes)
  )
import CiToolkit.Common.ProgramInput
  ( CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
  )
import CiToolkit.Common.ProgramOutcome
  ( ProgramOutcome(Failure, Success)
  , ProgramOutput(TextOutput)
  )
import CiToolkit.Common.Query (findLastCommit)
import CiToolkit.Common.Text.SerDe (serialize)
import CiToolkit.Common.Update (Update(MarkWithCIStage), markCommit)
import CiToolkit.Pipeline.Command
  ( Command(MarkCommit, GetLast, Version)
  )
import Control.Plus (empty)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Node.Path (FilePath)

execute ∷ String → ProgramInput Command → Aff ProgramOutcome
execute version (ProgramInput (CommonOptions commonOpts) command) =
  case command of
    GetLast (GetLastOptions { ciStagePrefix, ciStages }) → do
      repo ← loadRepo commonOpts.gitDirectory (pure ciStagePrefix)
      pure $ case findLastCommit ciStages repo of
        Just (commitRef /\ _) →
          Success
            { stderr: empty
            , stdout: pure $ TextOutput $ serialize FullHex commitRef
            }
        Nothing →
          Failure { exitCode: 1, stderr: "No such a commit." }
    MarkCommit
      (MarkCommitOptions { ciStage, ciStagePrefix, commitRef, dryRun }) →
      do
        repo ← loadRepo commonOpts.gitDirectory (pure ciStagePrefix)
        case markCommit ciStage commitRef repo of
          Just update → do
            if dryRun then
              pure $ Success
                { stderr: pure "Dry run mode.", stdout: empty }
            else do
              executeUpdate
                commonOpts.gitDirectory
                ciStagePrefix
                update

              pure $ Success
                { stderr: pure $ serialize unit update, stdout: empty }

          Nothing →
            let
              (CIStage stage) = ciStage

            in
              pure $ Success
                { stderr: pure $
                    "Commit '" <> (serialize FullHex commitRef)
                      <> "' is already marked with CI stage '"
                      <> NES.toString stage
                      <> "'"
                , stdout: empty
                }
    Version → executeVersion version

executeUpdate ∷ FilePath → CIStagePrefix → Update → Aff Unit
executeUpdate gitDirectory (CIStagePrefix prefix) update =
  case update of
    MarkWithCIStage commitRef (CIStage stage) →
      appendCommitNotes gitDirectory commitRef
        ( Notes $
            (NES.toString prefix <> NES.toString stage) : Nil
        )
