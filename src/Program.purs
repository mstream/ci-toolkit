module Program (execute) where

import Prelude
import CI
  ( CIStage(CIStage)
  , CIStagePrefix(CIStagePrefix)
  , PrintRepoOpts(PrintRepoOpts)
  , loadRepo
  )
import Data.Argonaut (encodeJson)
import Data.DotLang (toGraph)
import Data.List (List(Nil), (:))
import Data.String.NonEmpty as NES
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Git (appendCommitNotes)
import Git.Commit (Notes(Notes))
import Node.Path (FilePath)
import Query (findLastCommit)
import Print (showToHuman)
import ProgramInput
  ( Command(Render, MarkCommit, GetLast)
  , CommonOptions(CommonOptions)
  , GetLastOptions(GetLastOptions)
  , MarkCommitOptions(MarkCommitOptions)
  , ProgramInput(ProgramInput)
  , RenderFormat(DOT, JSON, Text)
  , RenderOptions(RenderOptions)
  )
import ProgramOutput (ProgramOutput)
import ProgramOutput as ProgramOutput
import Update (Update(MarkWithCIStage), markCommit)

execute ∷ ProgramInput → Aff ProgramOutput
execute (ProgramInput (CommonOptions commonOpts) command) = do
  repo ← loadRepo
    commonOpts.gitDirectory
    commonOpts.ciPrefix

  case command of

    GetLast (GetLastOptions _) →
      case findLastCommit commonOpts.ciStages repo of
        Just (commitRef /\ _) →
          pure $ ProgramOutput.Text $ showToHuman unit commitRef
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

          pure $ ProgramOutput.Text $ showToHuman unit update

        Nothing → do
          let
            (CIStage stage) = ciStage

          pure $ ProgramOutput.Text $
            "Commit '" <> (showToHuman unit commitRef)
              <> "' is already marked with CI stage '"
              <> NES.toString stage
              <> "'"

    Render (RenderOptions { format, limit }) → do
      case format of
        DOT → pure $ ProgramOutput.DOT $ toGraph repo
        JSON → pure $ ProgramOutput.JSON $ encodeJson repo
        Text → pure $ ProgramOutput.Text $ showToHuman
          ( PrintRepoOpts
              { ciStagesOrder: commonOpts.ciStages
              , commitsLimit: limit
              }
          )
          repo

executeUpdate ∷ FilePath → CIStagePrefix → Update → Aff Unit
executeUpdate gitDirectory (CIStagePrefix prefix) update =
  case update of
    MarkWithCIStage commitRef (CIStage stage) →
      appendCommitNotes gitDirectory commitRef
        ( Notes $
            (NES.toString prefix <> NES.toString stage) : Nil
        )
