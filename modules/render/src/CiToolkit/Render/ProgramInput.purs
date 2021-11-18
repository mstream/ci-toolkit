module CiToolkit.Render.ProgramInput
  ( Command(..)
  , commandParser
  ) where

import Prelude

import CiToolkit.Common.CI
  ( CIStage
  , CIStagePrefix(..)
  )
import CiToolkit.Common.ProgramOutput (OutputFormat(JSON))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Node.Path (FilePath)
import Options.Applicative as Opts
import Type.Proxy (Proxy(Proxy))

data Command
  = Branch
  | Commit
  | Repo

derive instance Generic Command _

instance Show Command where
  show = genericShow

commandParser ∷ Opts.Parser Command
commandParser = Opts.hsubparser $
  Opts.command "branch"
    (Opts.info branchCommandParser (Opts.progDesc "Render branch"))
    <> Opts.command "commit"
      (Opts.info commitCommandParser (Opts.progDesc "Render commit"))
    <> Opts.command "repo"
      ( Opts.info repoCommandParser
          (Opts.progDesc "Render repository")
      )

branchCommandParser ∷ Opts.Parser Command
branchCommandParser = pure Branch

commitCommandParser ∷ Opts.Parser Command
commitCommandParser = pure Branch

repoCommandParser ∷ Opts.Parser Command
repoCommandParser = pure Branch
