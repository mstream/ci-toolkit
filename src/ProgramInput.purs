module ProgramInput
  ( Command(..)
  , CommonOptions(..)
  , GetLastOptions(..)
  , MarkCommitOptions(..)
  , ProgramInput(..)
  , RenderFormat(..)
  , RenderOptions(..)
  , programInput
  ) where

import Prelude
import CI
  ( CIStage
  , CIStagePrefix(..)
  , ciPrefixParser
  , ciStageParser
  )
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Git.Commit (CommitRef, commitRefParser)
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
  | MarkCommit MarkCommitOptions
  | Render RenderOptions

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

programInput ∷ Opts.Parser ProgramInput
programInput = ado
  opts ← commonOptions
  cmd ← Opts.hsubparser $
    Opts.command "get-last"
      (Opts.info getLastCommand (Opts.progDesc "Get last commit"))
      <> Opts.command "mark-commit"
        (Opts.info markCommitCommand (Opts.progDesc "Mark commit"))
      <> Opts.command "render"
        (Opts.info renderCommand (Opts.progDesc "Render repository"))
  in ProgramInput opts cmd

getLastCommand ∷ Opts.Parser Command
getLastCommand = ado
  opts ← getLastOptions
  in GetLast opts

markCommitCommand ∷ Opts.Parser Command
markCommitCommand = ado
  opts ← getMarkCommitOptions
  in MarkCommit opts

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
  dryRun ← Opts.switch (Opts.long "dry-run")
  gitDirectory ← Opts.strOption
    (Opts.long "git-directory" <> Opts.value ".")
  isVerbose ← Opts.switch
    (Opts.long "verbose" <> Opts.short 'v')
  in CommonOptions { ciPrefix, dryRun, gitDirectory, isVerbose }

getLastOptions ∷ Opts.Parser GetLastOptions
getLastOptions = ado
  ciStages ← Opts.many $ Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "stage passed by a commit")
  in GetLastOptions { ciStages }

getMarkCommitOptions ∷ Opts.Parser MarkCommitOptions
getMarkCommitOptions = ado
  ciStage ← Opts.option parseCIStage
    (Opts.long "ci-stage" <> Opts.help "stage to be marked with")
  commitRef ← Opts.option parseCommitRef
    (Opts.long "commit-ref" <> Opts.help "commit to be marked")
  in MarkCommitOptions { ciStage, commitRef }

renderOptions ∷ Opts.Parser RenderOptions
renderOptions = ado
  format ← Opts.option
    parseRenderFormat
    (Opts.long "format" <> Opts.value JSON)
  in RenderOptions { format }

parseCIPrefix ∷ Opts.ReadM CIStagePrefix
parseCIPrefix = Opts.eitherReader $ \s →
  case runParser ciPrefixParser s of
    Left { error } → Left error
    Right ciPrefix → pure ciPrefix

parseCIStage ∷ Opts.ReadM CIStage
parseCIStage = Opts.eitherReader $ \s →
  case runParser ciStageParser s of
    Left { error } → Left error
    Right ciStage → pure ciStage

parseCommitRef ∷ Opts.ReadM CommitRef
parseCommitRef = Opts.eitherReader $ \s →
  case runParser commitRefParser s of
    Left { error } → Left error
    Right commitRef → pure commitRef

parseRenderFormat ∷ Opts.ReadM RenderFormat
parseRenderFormat = Opts.eitherReader $ \s →
  case s of
    "json" → pure JSON
    _ → Left "unsupported format"
