module ProgramInput
  ( Command(..)
  , CommonOptions(..)
  , GetLastOptions(..)
  , ProgramInput(..)
  , RenderFormat(..)
  , RenderOptions(..)
  , programInput
  ) where

import Prelude
import CI (CIStagePrefix(..), ciPrefixParser)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Node.Path (FilePath)
import Options.Applicative as Opts
import Text.Parsing.StringParser (runParser)
import Type.Proxy (Proxy(Proxy))

data ProgramInput = ProgramInput CommonOptions Command

derive instance Generic ProgramInput _

instance Show ProgramInput where
  show = genericShow

data Command
  = GetLast GetLastOptions
  | Render RenderOptions

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

newtype GetLastOptions = GetLastOptions
  { ciStages ∷ List String
  }

derive instance Generic GetLastOptions _

instance Show GetLastOptions where
  show = genericShow

newtype RenderOptions = RenderOptions
  { format ∷ RenderFormat
  }

derive instance Generic RenderOptions _

instance Show RenderOptions where
  show = genericShow

data RenderFormat =
  JSON

derive instance Generic RenderFormat _

instance Show RenderFormat where
  show = genericShow

parseCIPrefix ∷ Opts.ReadM CIStagePrefix
parseCIPrefix = Opts.eitherReader $ \s →
  case runParser ciPrefixParser s of
    Left { error } → Left error
    Right ciPrefix → pure ciPrefix

parseRenderFormat ∷ Opts.ReadM RenderFormat
parseRenderFormat = Opts.eitherReader $ \s →
  case s of
    "json" → pure JSON
    _ → Left "unsupported format"

programInput ∷ Opts.Parser ProgramInput
programInput = ado
  opts ← commonOptions
  cmd ← Opts.hsubparser $
    Opts.command "get-last"
      (Opts.info getLastCommand (Opts.progDesc "Get last commit"))
      <> Opts.command "render"
        (Opts.info renderCommand (Opts.progDesc "Render repository"))
  in ProgramInput opts cmd

getLastCommand ∷ Opts.Parser Command
getLastCommand = ado
  opts ← getLastOptions
  in GetLast opts

renderCommand ∷ Opts.Parser Command
renderCommand = ado
  opts ← renderOptions
  in Render opts

commonOptions ∷ Opts.Parser CommonOptions
commonOptions = ado
  ciPrefix ← Opts.option
    parseCIPrefix
    ( Opts.long "ci-prefix" <>
        (Opts.value $ CIStagePrefix (nes (Proxy ∷ Proxy "ci-")))
    )
  gitDirectory ← Opts.strOption
    (Opts.long "git-directory" <> Opts.value ".")
  isVerbose ← Opts.switch
    (Opts.long "verbose" <> Opts.short 'v')
  in CommonOptions { ciPrefix, gitDirectory, isVerbose }

getLastOptions ∷ Opts.Parser GetLastOptions
getLastOptions = ado
  ciStages ← Opts.many $ Opts.strOption $
    Opts.long "ci-stage" <> Opts.help "stage passed by a commit"
  in GetLastOptions { ciStages }

renderOptions ∷ Opts.Parser RenderOptions
renderOptions = ado
  format ← Opts.option
    parseRenderFormat
    (Opts.long "format" <> Opts.value JSON)
  in RenderOptions { format }
