module ProgramInput
  ( Command(..)
  , CommonOptions(..)
  , ProgramInput(..)
  , RenderOptions(..)
  , programInput
  ) where

import Prelude
import CI (CIStagePrefix, ciPrefixParser)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Node.Path (FilePath)
import Options.Applicative as Opts
import Text.Parsing.StringParser
  ( Parser
  , fail
  , runParser
  )

data ProgramInput = ProgramInput CommonOptions Command

derive instance Generic ProgramInput _

instance Show ProgramInput where
  show = genericShow

data Command =
  Render RenderOptions

derive instance Generic Command _

instance Show Command where
  show = genericShow

newtype CommonOptions = CommonOptions
  { ciPrefix ∷ CIStagePrefix
  , gitDirectory ∷ FilePath
  , isVerbose ∷ Boolean
  }

derive instance Generic CommonOptions _

instance Show CommonOptions where
  show = genericShow

newtype RenderOptions = RenderOptions
  { ciStages ∷ List String
  }

derive instance Generic RenderOptions _

instance Show RenderOptions where
  show = genericShow

parseCIPrefix ∷ Opts.ReadM CIStagePrefix
parseCIPrefix = Opts.eitherReader $ \s →
  case runParser ciPrefixParser s of
    Left { error } → Left error
    Right ciPrefix → pure ciPrefix

programInput ∷ Opts.Parser ProgramInput
programInput = ado
  opts ← commonOptions
  cmd ← Opts.hsubparser $ Opts.command
    "render"
    (Opts.info renderCommand (Opts.progDesc "Render repository"))
  in ProgramInput opts cmd

renderCommand ∷ Opts.Parser Command
renderCommand = ado
  opts ← renderOptions
  in Render opts

commonOptions ∷ Opts.Parser CommonOptions
commonOptions = ado
  ciPrefix ← Opts.option parseCIPrefix (Opts.long "ci-prefix")
  gitDirectory ← Opts.strOption $
    Opts.long "git-directory" <> Opts.value "."
  isVerbose ← Opts.switch $
    Opts.long "verbose" <> Opts.short 'v'
  in CommonOptions { ciPrefix, gitDirectory, isVerbose }

renderOptions ∷ Opts.Parser RenderOptions
renderOptions = ado
  ciStages ← Opts.many $ Opts.strOption $ Opts.long "ci-stage"
  in RenderOptions { ciStages }
