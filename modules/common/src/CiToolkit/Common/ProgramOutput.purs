module CiToolkit.Common.ProgramOutput
  ( OutputFormat(..)
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
import Data.Show.Generic (genericShow)

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
