module Print (class Printable, showToHuman, stringInColumn) where

import Prelude
import Data.Array (replicate)
import Data.Int (even)
import Data.String (joinWith, length)
import Data.Tuple.Nested ((/\))

class Printable a o where
  showToHuman ∷ o → a → String

stringInColumn ∷ Int → String → String
stringInColumn width s =
  let
    l = width - (length s)
    leftPaddingLen /\ rightPaddingLen =
      if even l then (l / 2) /\ (l / 2)
      else (l / 2) /\ (l / 2 + 1)
    padding w = joinWith "" (replicate w " ")
  in
    padding leftPaddingLen <> s <> padding rightPaddingLen
