module Main where

import Prelude
import CI (loadRepo)
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
  ( Command(GetLast, Render)
  , CommonOptions(CommonOptions)
  , GetLastOptions(GetLastOptions)
  , ProgramInput(ProgramInput)
  , RenderFormat(JSON)
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
  case command of

    GetLast (GetLastOptions cmdOpts) → do
      liftEffect $ log $ stringify $ encodeJson $ "TODO"

    Render (RenderOptions cmdOpts) → do
      repo ← loadRepo commonOptions.gitDirectory commonOptions.ciPrefix
      let
        output = case cmdOpts.format of
          JSON → stringify $ encodeJson repo

      liftEffect $ log output
