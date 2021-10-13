module Main where

import Prelude
import CLI (run)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Git (getCommitInfo, getCommitNotes, getCommitRefs)
import Query (findLastCommit)
import Options.Applicative as Opts
import Options.Applicative ((<**>))
import ProgramInput
  ( Command(GetLast, MarkCommit, Render)
  , CommonOptions(CommonOptions)
  , GetLastOptions(GetLastOptions)
  , MarkCommitOptions(MarkCommitOptions)
  , ProgramInput(ProgramInput)
  , RenderFormat(JSON)
  , RenderOptions(RenderOptions)
  , programInput
  )
import Update (markCommit)

main ∷ Effect Unit
main = launchAff_ run
