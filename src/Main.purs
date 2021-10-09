module Main where

import Prelude
import Data.Argonaut (encodeJson, stringify)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Git (getCommitInfo, getCommitNotes, getCommitRefs)
import Options.Applicative as Opts
import Options.Applicative ((<**>))
import ProgramInput
  ( Command(Render)
  , CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
  , RenderOptions(RenderOptions)
  , programInput
  )

main ∷ Effect Unit
main = do
  args ← Opts.execParser options
  launchAff_ $ run args

options ∷ Opts.ParserInfo ProgramInput
options = Opts.info (programInput <**> Opts.helper) (Opts.fullDesc)

run ∷ ProgramInput → Aff Unit
run (ProgramInput (CommonOptions commonOptions) command) = do
  commitRefs ← getCommitRefs commonOptions.gitDirectory
  commitRefsWithNotes ← traverse
    ( \ref → getCommitNotes commonOptions.gitDirectory ref >>= \notes →
        pure { ref, notes }
    )
    commitRefs
  commitRefsWithNotesAndInfo ← traverse
    ( \commit → do
        commitInfo ← getCommitInfo commonOptions.gitDirectory commit.ref
        pure { info: commitInfo, notes: commit.notes, ref: commit.ref }
    )
    commitRefsWithNotes
  case command of
    Render (RenderOptions renderOptions) → do
      liftEffect $ log $ show $ renderOptions.ciStages
      liftEffect $ log $ stringify $ encodeJson
        commitRefsWithNotesAndInfo
