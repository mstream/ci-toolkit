module CiToolkit.Common.CLI (run) where

import Prelude

import CiToolkit.Common.ProgramOutput (ProgramOutput)
import CiToolkit.Common.Text.SerDe (serialize)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Options.Applicative ((<**>))
import Options.Applicative as Opts

run ∷ ∀ i. Opts.Parser i → (i → Aff ProgramOutput) → Aff Unit
run programInputParser execute = do
  args ← liftEffect $ Opts.execParser $ options programInputParser
  output ← execute args
  liftEffect $ log $ serialize unit output

options ∷ ∀ i. Opts.Parser i → Opts.ParserInfo i
options programInputParser = Opts.info
  (programInputParser <**> Opts.helper)
  (Opts.fullDesc)
