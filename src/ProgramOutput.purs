module ProgramOutput
  ( ProgramOutput(..)
  ) where

import Prelude
import Data.Argonaut (Json, stringify)
import Data.DotLang (Graph)
import Data.DotLang.Class (toText)
import Data.Generic.Rep (class Generic)

data ProgramOutput
  = DOT Graph
  | JSON Json
  | Text String

derive instance Generic ProgramOutput _

instance Show ProgramOutput where
  show = case _ of
    DOT graph → toText graph
    JSON json → stringify json
    Text s → s

instance Eq ProgramOutput where
  eq o1 o2 = show o1 == show o2
