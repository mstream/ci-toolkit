module CiToolkit.Render.Program (execute) where

import Prelude

import CiToolkit.Common.CI
  ( RenderRepoOpts(RenderRepoOpts)
  , loadRepo
  )
import CiToolkit.Common.ProgramInput
  ( CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
  )
import CiToolkit.Common.ProgramOutput
  ( OutputFormat(DOT, JSON, Text)
  , ProgramOutput(DotOutput, JsonOutput, TextOutput)
  )
import CiToolkit.Common.Text.SerDe (serialize)
import CiToolkit.Render.ProgramInput
  ( Command(Branch, Commit, Repo, Version)
  , RepoOptions(RepoOptions)
  )
import Data.Argonaut (encodeJson)
import Data.DotLang (toGraph)
import Effect.Aff (Aff)

execute ∷ String → ProgramInput Command → Aff ProgramOutput
execute version (ProgramInput (CommonOptions commonOpts) command) = do
  case command of
    Branch → pure $ TextOutput $ "TODO"
    Commit → pure $ TextOutput $ "TODO"
    Repo (RepoOptions { ciStagePrefix, ciStages, outputFormat }) → do
      repo ← loadRepo commonOpts.gitDirectory (pure ciStagePrefix)
      pure $ case outputFormat of
        DOT → DotOutput $ toGraph repo
        JSON → JsonOutput $ encodeJson repo
        Text → TextOutput $ serialize
          ( RenderRepoOpts
              { ciStagesOrder: ciStages, commitsLimit: 0 }
          )
          repo
    Version → pure $ TextOutput version
