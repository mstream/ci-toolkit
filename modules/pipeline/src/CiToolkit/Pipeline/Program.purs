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
import CiToolkit.Pipeline.ProgramInput
  ( Command(MarkCommit, GetLast)
  , GetLastOptions(GetLastOptions)
  , MarkCommitOptions(MarkCommitOptions)
  )
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Node.Path (FilePath)

execute ∷ ProgramInput Command → Aff ProgramOutput
execute (ProgramInput (CommonOptions commonOpts) command) = do
  repo ← loadRepo
    commonOpts.gitDirectory
    commonOpts.ciPrefix

  case command of

    GetLast (GetLastOptions { ciStages }) →
      case findLastCommit ciStages repo of
        Just (commitRef /\ _) →
          pure $ TextOutput $ serialize FullHex commitRef
        Nothing → pure $ TextOutput $ "Not found."

    MarkCommit (MarkCommitOptions { ciStage, commitRef }) →
      case markCommit ciStage commitRef repo of
        Just update → do

          if commonOpts.dryRun then pure unit
          else
            executeUpdate
              commonOpts.gitDirectory
              commonOpts.ciPrefix
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

executeUpdate ∷ FilePath → CIStagePrefix → Update → Aff Unit
executeUpdate gitDirectory (CIStagePrefix prefix) update =
  case update of
    MarkWithCIStage commitRef (CIStage stage) →
      appendCommitNotes gitDirectory commitRef
        ( Notes $
            (NES.toString prefix <> NES.toString stage) : Nil
        )
