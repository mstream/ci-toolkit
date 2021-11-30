module CiToolkit.Pipeline.Command
  ( Command(..)
  , commandParser
  ) where

import Prelude

import CiToolkit.Common.Command.Version
  ( versionCommandDescription
  )
import CiToolkit.Common.Documentation (commandOptionDescription)
import CiToolkit.Common.Documentation.Pipeline.GetLast
  ( GetLastOptions
  , getLastOptionsParser
  )
import CiToolkit.Common.Documentation.Pipeline.MarkCommit
  ( MarkCommitOptions
  , markCommitOptionsParser
  )
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Options.Applicative as Opts
import Options.Applicative.Types (Parser(NilP))

data Command
  = GetLast GetLastOptions
  | MarkCommit MarkCommitOptions
  | Version

derive instance Generic Command _

instance Show Command where
  show = genericShow

commandParser ∷ Opts.Parser Command
commandParser = Opts.hsubparser $
  Opts.command
    "get-last"
    ( Opts.info
        (GetLast <$> getLastOptionsParser)
        (Opts.progDesc "TODO")
    )
    <>
      Opts.command
        "mark-commit"
        ( Opts.info
            (MarkCommit <$> markCommitOptionsParser)
            (Opts.progDesc "TODO")
        )
    <>
      Opts.command
        "version"
        ( Opts.info
            (NilP Version)
            (Opts.progDesc versionCommandDescription)
        )
