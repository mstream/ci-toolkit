module CiToolkit.Version.Command
  ( Command(..)
  , commandParser
  ) where

import Prelude

import CiToolkit.Common.Command.Version
  ( versionCommandDescription
  )
import CiToolkit.Version.Command.Show
  ( ShowOptions
  , showCommandDescription
  , showOptionsParser
  )
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Options.Applicative as Opts
import Options.Applicative.Types (Parser(NilP))

data Command
  = Show ShowOptions
  | Version

derive instance Generic Command _

instance Show Command where
  show = genericShow

commandParser ∷ Opts.Parser Command
commandParser =
  Opts.hsubparser $
    Opts.command
      "show"
      ( Opts.info
          (Show <$> showOptionsParser)
          (Opts.progDesc showCommandDescription)
      )
      <>
        Opts.command
          "version"
          ( Opts.info
              (NilP Version)
              (Opts.progDesc versionCommandDescription)
          )
