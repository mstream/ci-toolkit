module CiToolkit.Common.Text.SerDe
  ( class Deserializable
  , class Serializable
  , deserialize
  , serialize
  ) where

import Data.Either.Nested (type (\/))
import Text.Parsing.StringParser (ParseError)

class Serializable a o where
  serialize ∷ o → a → String

class Deserializable a o where
  deserialize ∷ o → String → ParseError \/ a
