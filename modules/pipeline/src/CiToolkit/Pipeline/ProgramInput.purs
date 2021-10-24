module CiToolkit.Pipeline.ProgramInput
  ( Command(..)
  , CommonOptions(..)
  , GetLastOptions(..)
  , MarkCommitOptions(..)
  , ProgramInput(..)
  , programInputParser
  ) where

import Prelude

import CiToolkit.Common.CI (CIStage, CIStagePrefix(..))
import CiToolkit.Common.Git.Commit (CommitRef)
import CiToolkit.Common.ProgramInput
  ( parseCIPrefix
  , parseCIStage
  , parseCommitRef
  )
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
  = GetLast GetLastOptions
  | MarkCommit MarkCommitOptions

derive instance Generic Command _

instance Show Command where
  show = genericShow

newtype CommonOptions = CommonOptions
  { ciPrefix ∷ CIStagePrefix
  , dryRun ∷ Boolean
  , gitDirectory ∷ FilePath
  , isVerbose ∷ Boolean
  }

derive instance Generic CommonOptions _

instance Show CommonOptions where
  show = genericShow

newtype GetLastOptions = GetLastOptions
  { ciStages ∷ List CIStage
  }

newtype MarkCommitOptions = MarkCommitOptions
  { ciStage ∷ CIStage
  , commitRef ∷ CommitRef
  }

derive instance Generic MarkCommitOptions _

instance Show MarkCommitOptions where
  show = genericShow

derive instance Generic GetLastOptions _

instance Show GetLastOptions where
  show = genericShow

programInputParser ∷ Opts.Parser ProgramInput
programInputParser = ado
  opts ← commonOptionsParser
  cmd ← Opts.hsubparser $
    Opts.command "get-last"
      (Opts.info getLastCommandParser (Opts.progDesc "Get last commit"))
      <> Opts.command "mark-commit"
        ( Opts.info markCommitCommandParser
            (Opts.progDesc "Mark commit")
        )
  in ProgramInput opts cmd

getLastCommandParser ∷ Opts.Parser Command
getLastCommandParser = ado
  opts ← getLastOptionsParser
  in GetLast opts

markCommitCommandParser ∷ Opts.Parser Command
markCommitCommandParser = ado
  opts ← getMarkCommitOptionsParser
  in MarkCommit opts

commonOptionsParser ∷ Opts.Parser CommonOptions
commonOptionsParser = ado
  ciPrefix ← Opts.option
    parseCIPrefix
    ( Opts.long "ci-prefix" <>
        (Opts.value $ CIStagePrefix (nes (Proxy ∷ Proxy "ci-")))
    )
  dryRun ← Opts.switch (Opts.long "dry-run")
  gitDirectory ← Opts.strOption
    (Opts.long "git-directory" <> Opts.value ".")
  isVerbose ← Opts.switch
    (Opts.long "verbose" <> Opts.short 'v')

  in
    CommonOptions
      { ciPrefix
      , dryRun
      , gitDirectory
      , isVerbose
      }

getLastOptionsParser ∷ Opts.Parser GetLastOptions
getLastOptionsParser = ado
  ciStages ← Opts.many $ Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "passed stage")
  in GetLastOptions { ciStages }

getMarkCommitOptionsParser ∷ Opts.Parser MarkCommitOptions
getMarkCommitOptionsParser = ado
  ciStage ← Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "stage to be marked with")
  commitRef ← Opts.option parseCommitRef
    (Opts.long "commit-ref" <> Opts.help "commit to be marked")
  in MarkCommitOptions { ciStage, commitRef }
