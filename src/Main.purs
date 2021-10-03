module Main where

import Prelude
import Data.Argonaut (encodeJson, stringify)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Git (getCommitInfo, getCommitNotes, getCommitRefs)
import Options.Applicative as Opts
import Options.Applicative ((<**>))
import ProgramInput (ProgramInput, programInput)

main ∷ Effect Unit
main = do
  args ← Opts.execParser options
  run args

options ∷ Opts.ParserInfo ProgramInput
options = Opts.info (programInput <**> Opts.helper) (Opts.fullDesc)

run ∷ ProgramInput → Effect Unit
run _ = do
  commitRefs ← getCommitRefs
  commitRefsWithNotes ← traverse
    ( \ref → getCommitNotes ref >>= \notes →
        pure { ref, notes }
    )
    commitRefs
  commitRefsWithNotesAndInfo ← traverse
    ( \commit → getCommitInfo commit.ref >>= \info →
        pure { info, notes: commit.notes, ref: commit.ref }
    )
    commitRefsWithNotes
  log $ stringify $ encodeJson commitRefsWithNotesAndInfo
