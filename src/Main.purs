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
import ProgramInput (ProgramInput, programInput)

main ∷ Effect Unit
main = do
  args ← Opts.execParser options
  launchAff_ $ run args

options ∷ Opts.ParserInfo ProgramInput
options = Opts.info (programInput <**> Opts.helper) (Opts.fullDesc)

run ∷ ProgramInput → Aff Unit
run _ = do
  let gitDirPath = "."
  commitRefs ← getCommitRefs gitDirPath
  commitRefsWithNotes ← traverse
    ( \ref → getCommitNotes gitDirPath ref >>= \notes →
        pure { ref, notes }
    )
    commitRefs
  commitRefsWithNotesAndInfo ← traverse
    ( \commit → getCommitInfo gitDirPath commit.ref >>= \info →
        pure { info, notes: commit.notes, ref: commit.ref }
    )
    commitRefsWithNotes
  let output = stringify $ encodeJson commitRefsWithNotesAndInfo
  liftEffect $ log output
