module CiToolkit.Pipeline.Command
  ( Command(..)
  , commandParser
  ) where

import Prelude

import CiToolkit.Common.Command.Version (versionCommandDescription)
import CiToolkit.Pipeline.Command.GetLast
  ( GetLastOptions
  , getLastCommandDescription
  , getLastOptionsParser
  )
import CiToolkit.Pipeline.Command.MarkCommit
  ( MarkCommitOptions
  , markCommitCommandDescription
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
        (Opts.progDesc getLastCommandDescription)
    )
    <>
      Opts.command
        "mark-commit"
        ( Opts.info
            (MarkCommit <$> markCommitOptionsParser)
            (Opts.progDesc markCommitCommandDescription)
        )
    <>
      Opts.command
        "version"
        ( Opts.info
            (NilP Version)
            (Opts.progDesc versionCommandDescription)
        )
