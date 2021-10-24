module CiToolkit.Render.ProgramInput
  ( Command(..)
  , CommonOptions(..)
  , ProgramInput(..)
  , programInputParser
  ) where

import Prelude

import CiToolkit.Common.CI
  ( CIStage
  , CIStagePrefix(..)
  )
import CiToolkit.Common.ProgramInput
  ( parseCIPrefix
  , parseCIStage
  , parseOutputFormat
  )
import CiToolkit.Common.ProgramOutput (OutputFormat(JSON))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Node.Path (FilePath)
import Options.Applicative as Opts
import Type.Proxy (Proxy(Proxy))

data ProgramInput = ProgramInput CommonOptions Command

derive instance Generic ProgramInput _

instance Show ProgramInput where
  show = genericShow

data Command
  = Branch
  | Commit
  | Repo

derive instance Generic Command _

instance Show Command where
  show = genericShow

newtype CommonOptions = CommonOptions
  { ciPrefix ∷ CIStagePrefix
  , ciStages ∷ List CIStage
  , dryRun ∷ Boolean
  , gitDirectory ∷ FilePath
  , format ∷ OutputFormat
  , isVerbose ∷ Boolean
  }

derive instance Generic CommonOptions _

instance Show CommonOptions where
  show = genericShow

programInputParser ∷ Opts.Parser ProgramInput
programInputParser = ado
  opts ← commonOptionsParser
  cmd ← Opts.hsubparser $
    Opts.command "branch"
      (Opts.info branchCommandParser (Opts.progDesc "Render branch"))
      <> Opts.command "commit"
        (Opts.info commitCommandParser (Opts.progDesc "Render commit"))
      <> Opts.command "repo"
        ( Opts.info repoCommandParser
            (Opts.progDesc "Render repository")
        )

  in ProgramInput opts cmd

branchCommandParser ∷ Opts.Parser Command
branchCommandParser = pure Branch

commitCommandParser ∷ Opts.Parser Command
commitCommandParser = pure Branch

repoCommandParser ∷ Opts.Parser Command
repoCommandParser = pure Branch

commonOptionsParser ∷ Opts.Parser CommonOptions
commonOptionsParser = ado
  ciPrefix ← Opts.option
    parseCIPrefix
    ( Opts.long "ci-prefix" <>
        (Opts.value $ CIStagePrefix (nes (Proxy ∷ Proxy "ci-")))
    )
  ciStages ← Opts.many $ Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "passed stage")
  dryRun ← Opts.switch (Opts.long "dry-run")
  format ← Opts.option
    parseOutputFormat
    (Opts.long "format" <> Opts.value JSON)
  gitDirectory ← Opts.strOption
    (Opts.long "git-directory" <> Opts.value ".")
  isVerbose ← Opts.switch
    (Opts.long "verbose" <> Opts.short 'v')

  in
    CommonOptions
      { ciPrefix
      , ciStages
      , dryRun
      , format
      , gitDirectory
      , isVerbose
      }
