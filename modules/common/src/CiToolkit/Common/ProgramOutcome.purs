module CiToolkit.Common.ProgramOutcome
  ( OutputFormat(..)
  , ProgramOutcome(..)
  , ProgramOutput(..)
  ) where

import Prelude

import CiToolkit.Common.Text.SerDe (class Serializable, serialize)
import Data.Argonaut (Json, stringify)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.DotLang (Graph)
import Data.DotLang.Class (toText)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data ProgramOutcome
  = Failure { exitCode ∷ Int, stderr ∷ String }
  | Success { stderr ∷ Maybe String, stdout ∷ Maybe ProgramOutput }

derive instance Generic ProgramOutcome _

instance Show ProgramOutcome where
  show = genericShow

derive instance Eq ProgramOutcome

data ProgramOutput
  = DotOutput Graph
  | JsonOutput Json
  | TextOutput String

derive instance Generic ProgramOutput _

instance Show ProgramOutput where
  show = serialize unit

instance Eq ProgramOutput where
  eq x1 x2 = show x1 == show x2

instance Serializable ProgramOutput Unit where
  serialize _ = case _ of
    DotOutput graph → toText graph
    JsonOutput json → stringify json
    TextOutput text → text

data OutputFormat
  = DOT
  | JSON
  | Text

derive instance Generic OutputFormat _

instance Show OutputFormat where
  show = genericShow

instance EncodeJson OutputFormat where
  encodeJson = genericEncodeJson

instance Eq OutputFormat where
  eq = genericEq
