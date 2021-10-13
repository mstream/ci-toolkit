module ProgramOutput
  ( ProgramOutput(..)
  ) where

import Prelude
import CI (CIStage, CIStagePrefix(..), ciPrefixParser, ciStageParser)
import Data.Argonaut (Json, stringify)
import Data.Either (Either(Left, Right))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (nes)
import Git.Commit (CommitRef)
import Node.Path (FilePath)
import Options.Applicative as Opts
import Text.Parsing.StringParser (runParser)
import Type.Proxy (Proxy(Proxy))

data ProgramOutput
  = JSON Json
  | Text String

derive instance Generic ProgramOutput _

instance Show ProgramOutput where
  show = case _ of
    JSON json → stringify json
    Text s → s

instance Eq ProgramOutput where
  eq = genericEq
