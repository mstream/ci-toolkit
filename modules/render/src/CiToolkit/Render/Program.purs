module CiToolkit.Render.Program (execute) where

import Prelude

import CiToolkit.Common.CI
  ( RenderRepoOpts(RenderRepoOpts)
  , loadRepo
  )
import CiToolkit.Common.CLI (executeVersion)
import CiToolkit.Common.Documentation.Render.Branch
  ( BranchOptions(BranchOptions)
  )
import CiToolkit.Common.Documentation.Render.Commit
  ( CommitOptions(CommitOptions)
  )
import CiToolkit.Common.Documentation.Render.Repo
  ( RepoOptions(RepoOptions)
  )
import CiToolkit.Common.ProgramInput
  ( CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
  )
import CiToolkit.Common.ProgramOutcome
  ( OutputFormat(DOT, JSON, Text)
  , ProgramOutcome(Failure, Success)
  , ProgramOutput(DotOutput, JsonOutput, TextOutput)
  )
import CiToolkit.Common.Text.SerDe (serialize)
import CiToolkit.Render.Command (Command(Branch, Commit, Repo, Version))
import Control.Plus (empty)
import Data.Argonaut (encodeJson)
import Data.DotLang (toGraph)
import Effect.Aff (Aff)

execute ∷ String → ProgramInput Command → Aff ProgramOutcome
execute version (ProgramInput (CommonOptions commonOpts) command) = do
  case command of
    Branch _ → pure $ Success { stderr: pure "TODO", stdout: empty }
    Commit _ → pure $ Success { stderr: pure "TODO", stdout: empty }
    Repo (RepoOptions { ciStagePrefix, ciStages, outputFormat }) → do
      repo ← loadRepo commonOpts.gitDirectory (pure ciStagePrefix)
      pure $ case outputFormat of
        DOT → Success
          { stderr: empty, stdout: pure $ DotOutput $ toGraph repo }
        JSON → Success
          { stderr: empty, stdout: pure $ JsonOutput $ encodeJson repo }
        Text → Success
          { stderr: empty
          , stdout: pure $ TextOutput $ serialize
              ( RenderRepoOpts
                  { ciStagesOrder: ciStages, commitsLimit: 0 }
              )
              repo
          }
    Version → executeVersion version
