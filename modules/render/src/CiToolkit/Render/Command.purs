module CiToolkit.Render.Command
  ( Command(..)
  , commandParser
  ) where

import Prelude

import CiToolkit.Common.CI
  ( CIStage
  , CIStagePrefix(..)
  )
import CiToolkit.Common.Command.Version (versionCommandDescription)
import CiToolkit.Common.Documentation.Render.Branch
  ( BranchOptions
  , branchOptionsParser
  )
import CiToolkit.Common.Documentation.Render.Commit
  ( CommitOptions
  , commitOptionsParser
  )
import CiToolkit.Common.Documentation.Render.Repo
  ( RepoOptions
  , repoOptionsParser
  )
import CiToolkit.Common.ProgramInput
  ( ciStagePrefixParser
  , outputFormatParser
  , parseCIStage
  )
import CiToolkit.Common.ProgramOutcome (OutputFormat(JSON))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Node.Path (FilePath)
import Options.Applicative as Opts
import Options.Applicative.Types (Parser(NilP))
import Type.Proxy (Proxy(Proxy))

data Command
  = Branch BranchOptions
  | Commit CommitOptions
  | Repo RepoOptions
  | Version

derive instance Generic Command _

instance Show Command where
  show = genericShow

commandParser ∷ Opts.Parser Command
commandParser = Opts.hsubparser $
  Opts.command
    "branch"
    ( Opts.info
        (Branch <$> branchOptionsParser)
        (Opts.progDesc "TODO")
    )
    <>
      Opts.command
        "commit"
        ( Opts.info
            (Commit <$> commitOptionsParser)
            (Opts.progDesc "TODO")
        )
    <>
      Opts.command
        "repo"
        ( Opts.info
            (Repo <$> repoOptionsParser)
            (Opts.progDesc "TODO")
        )
    <>
      Opts.command
        "version"
        ( Opts.info
            (NilP Version)
            (Opts.progDesc versionCommandDescription)
        )
