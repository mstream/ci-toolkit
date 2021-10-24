module CiToolkit.Common.ProgramInput
  ( parseCIPrefix
  , parseCIStage
  , parseCommitRef
  , parseOutputFormat
  ) where

import Prelude

import CiToolkit.Common.CI
  ( CIStage
  , CIStagePrefix
  , ciPrefixParser
  , ciStageParser
  )
import CiToolkit.Common.Git.Commit (CommitRef, commitRefParser)
import CiToolkit.Common.ProgramOutput (OutputFormat(DOT, JSON, Text))
import Data.Either (Either(Left, Right))
import Options.Applicative as Opts
import Text.Parsing.StringParser (runParser)

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

parseOutputFormat ∷ Opts.ReadM OutputFormat
parseOutputFormat = Opts.eitherReader $ \s →
  case s of
    "dot" → pure DOT
    "json" → pure JSON
    "text" → pure Text
    _ → Left "unsupported format"
