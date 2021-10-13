module Print (class Printable, showToHuman) where

class Printable a where
  showToHuman ∷ a → String
