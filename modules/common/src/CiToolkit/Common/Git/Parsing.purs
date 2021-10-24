module CiToolkit.Common.Git.Parsing
  ( eolParser
  , fullStringParser
  , linesParser
  , wordParser
  ) where

import Prelude

import Data.List (List)
import Data.Maybe (maybe)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (regex, string)
import Text.Parsing.StringParser.Combinators (sepBy)

eolParser ∷ Parser Unit
eolParser = void $ string "\n"

fullStringParser ∷ Parser String
fullStringParser = regex ".*"

linesParser ∷ Parser (List String)
linesParser = fullStringParser `sepBy` eolParser

wordParser ∷ Parser NonEmptyString
wordParser = do
  word ← regex "[^\\s]+"
  maybe
    (fail "empty word")
    pure
    (NES.fromString word)
