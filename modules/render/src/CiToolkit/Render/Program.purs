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
  ( Command(Branch, Commit, Repo)
  )
import Data.Argonaut (encodeJson)
import Data.DotLang (toGraph)
import Effect.Aff (Aff)

execute ∷ ProgramInput Command → Aff ProgramOutput
execute (ProgramInput (CommonOptions commonOpts) command) = do
  case command of
    Branch → pure $ TextOutput $ "TODO"
    Commit → pure $ TextOutput $ "TODO"
    Repo → do
      repo ← loadRepo commonOpts.gitDirectory commonOpts.ciPrefix
      pure $ case commonOpts.format of
        DOT → DotOutput $ toGraph repo
        JSON → JsonOutput $ encodeJson repo
        Text → TextOutput $ serialize
          ( RenderRepoOpts
              { ciStagesOrder: commonOpts.ciStages, commitsLimit: 0 }
          )
          repo
