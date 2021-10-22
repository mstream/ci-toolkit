module Text.SerDe
  ( class Deserializable
  , class Serializable
  , deserialize
  , serialize
  ) where

import Prelude
import Data.Array (replicate)
import Data.Either.Nested (type (\/))
import Data.Int (even)
import Data.String (joinWith, length)
import Data.Tuple.Nested ((/\))
import Text.Parsing.StringParser (ParseError)

class Serializable a o where
  serialize ∷ o → a → String

class Deserializable a o where
  deserialize ∷ o → String → ParseError \/ a
