module CiToolkit.Pipeline.Program (execute) where

import Prelude

import CiToolkit.Common.CI
  ( CIStage(CIStage)
  , CIStagePrefix(CIStagePrefix)
  , loadRepo
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
import CiToolkit.Common.ProgramOutput (ProgramOutput(TextOutput))
import CiToolkit.Common.Query (findLastCommit)
import CiToolkit.Common.Text.SerDe (serialize)
import CiToolkit.Common.Update (Update(MarkWithCIStage), markCommit)
import CiToolkit.Pipeline.Command
  ( Command(MarkCommit, GetLast, Version)
  )
import CiToolkit.Pipeline.Command.GetLast
  ( GetLastOptions(GetLastOptions)
  )
import CiToolkit.Pipeline.Command.MarkCommit
  ( MarkCommitOptions(MarkCommitOptions)
  )
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Node.Path (FilePath)

execute ∷ String → ProgramInput Command → Aff ProgramOutput
execute version (ProgramInput (CommonOptions commonOpts) command) =
  case command of
    GetLast (GetLastOptions { ciStagePrefix, ciStages }) → do
      repo ← loadRepo commonOpts.gitDirectory (pure ciStagePrefix)
      pure $ case findLastCommit ciStages repo of
        Just (commitRef /\ _) →
          TextOutput $ serialize FullHex commitRef
        Nothing →
          TextOutput $ "Not found."
    MarkCommit
      (MarkCommitOptions { ciStage, ciStagePrefix, commitRef, dryRun }) →
      do
        repo ← loadRepo commonOpts.gitDirectory (pure ciStagePrefix)
        case markCommit ciStage commitRef repo of
          Just update → do
            if dryRun then pure unit
            else
              executeUpdate
                commonOpts.gitDirectory
                ciStagePrefix
                update

            pure $ TextOutput $ serialize unit update

          Nothing → do
            let
              (CIStage stage) = ciStage

            pure $ TextOutput $
              "Commit '" <> (serialize FullHex commitRef)
                <> "' is already marked with CI stage '"
                <> NES.toString stage
                <> "'"
    Version → pure $ TextOutput version

executeUpdate ∷ FilePath → CIStagePrefix → Update → Aff Unit
executeUpdate gitDirectory (CIStagePrefix prefix) update =
  case update of
    MarkWithCIStage commitRef (CIStage stage) →
      appendCommitNotes gitDirectory commitRef
        ( Notes $
            (NES.toString prefix <> NES.toString stage) : Nil
        )
