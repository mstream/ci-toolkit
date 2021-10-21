module CLI (run) where

import Prelude
import Data.Argonaut (stringify)
import Data.DotLang.Class (toText)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Options.Applicative as Opts
import Options.Applicative ((<**>))
import Program (execute)
import ProgramInput (ProgramInput, programInput)
import ProgramOutput as ProgramOutput

run ∷ Aff Unit
run = do
  args ← liftEffect $ Opts.execParser options
  output ← execute args
  liftEffect $ log $ case output of
    ProgramOutput.DOT graph → toText graph
    ProgramOutput.JSON json → stringify json
    ProgramOutput.Text s → s

options ∷ Opts.ParserInfo ProgramInput
options = Opts.info (programInput <**> Opts.helper) (Opts.fullDesc)
