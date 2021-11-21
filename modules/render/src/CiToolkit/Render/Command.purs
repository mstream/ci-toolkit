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
import CiToolkit.Common.ProgramInput
  ( ciStagePrefixParser
  , outputFormatParser
  , parseCIStage
  )
import CiToolkit.Common.ProgramOutput (OutputFormat(JSON))
import CiToolkit.Render.Command.Branch
  ( branchCommandDescription
  )
import CiToolkit.Render.Command.Commit
  ( commitCommandDescription
  )
import CiToolkit.Render.Command.Repo
  ( RepoOptions(RepoOptions)
  , repoCommandDescription
  , repoOptionsParser
  )
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Node.Path (FilePath)
import Options.Applicative as Opts
import Options.Applicative.Types (Parser(NilP))
import Type.Proxy (Proxy(Proxy))

data Command
  = Branch
  | Commit
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
        (NilP Branch)
        (Opts.progDesc branchCommandDescription)
    )
    <>
      Opts.command
        "commit"
        ( Opts.info
            (NilP Commit)
            (Opts.progDesc commitCommandDescription)
        )
    <>
      Opts.command
        "repo"
        ( Opts.info
            (Repo <$> repoOptionsParser)
            (Opts.progDesc repoCommandDescription)
        )
    <>
      Opts.command
        "version"
        ( Opts.info
            (NilP Version)
            (Opts.progDesc versionCommandDescription)
        )
