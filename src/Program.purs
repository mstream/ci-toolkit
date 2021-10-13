module Program (execute) where

import Prelude
import CI (CIStage(CIStage), CIStagePrefix(CIStagePrefix), loadRepo)
import Data.Argonaut (encodeJson)
import Data.List (List(Nil), (:))
import Data.String.NonEmpty as NES
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Git (appendCommitNotes)
import Git.Commit (Notes(Notes), asHex)
import Node.Path (FilePath)
import Query (findLastCommit)
import Print (showToHuman)
import ProgramInput
  ( Command(Render, MarkCommit, GetLast)
  , CommonOptions(CommonOptions)
  , GetLastOptions(GetLastOptions)
  , MarkCommitOptions(MarkCommitOptions)
  , ProgramInput(ProgramInput)
  , RenderFormat(JSON)
  , RenderOptions(RenderOptions)
  )
import ProgramOutput (ProgramOutput)
import ProgramOutput as ProgramOutput
import Update (Update(MarkWithCIStage), markCommit)

execute ∷ ProgramInput → Aff ProgramOutput
execute (ProgramInput (CommonOptions commonOpts) command) = do
  repo ← loadRepo commonOpts.gitDirectory commonOpts.ciPrefix
  case command of

    GetLast (GetLastOptions cmdOpts) →
      case findLastCommit cmdOpts.ciStages repo of
        Just (commitRef /\ _) →
          pure $ ProgramOutput.Text $ asHex commitRef
        Nothing → pure $ ProgramOutput.Text $ "Not found."

    MarkCommit (MarkCommitOptions { ciStage, commitRef }) →
      case markCommit ciStage commitRef repo of
        Just update → do

          if commonOpts.dryRun then pure unit
          else
            executeUpdate
              commonOpts.gitDirectory
              commonOpts.ciPrefix
              update

          pure $ ProgramOutput.Text $ showToHuman update

        Nothing → do
          let
            (CIStage stage) = ciStage

          pure $ ProgramOutput.Text $
            "Commit '" <> asHex commitRef
              <> "' is already marked with CI stage '"
              <> NES.toString stage
              <> "'"

    Render (RenderOptions cmdOpts) → do
      case cmdOpts.format of
        JSON → pure $ ProgramOutput.JSON $ encodeJson repo

executeUpdate ∷ FilePath → CIStagePrefix → Update → Aff Unit
executeUpdate gitDirectory (CIStagePrefix prefix) update =
  case update of
    MarkWithCIStage commitRef (CIStage stage) →
      appendCommitNotes gitDirectory commitRef
        ( Notes $
            (NES.toString prefix <> NES.toString stage) : Nil
        )
