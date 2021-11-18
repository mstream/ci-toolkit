module Test.CiToolkit.Common.Print (spec) where

import Prelude

import CiToolkit.Common.Print (stringInColumn)
import Data.Array (replicate)
import Data.String (joinWith, length)
import Data.String.Gen (genAsciiString)
import Effect.Class (liftEffect)
import Test.CiToolkit.Common.TestUtils (toResult)
import Test.QuickCheck (quickCheckGen')
import Test.QuickCheck.Gen (chooseInt)
import Test.Spec (Spec, describe, it)

spec ∷ Spec Unit
spec = describe "Print" do
  stringInColumnSpec

stringInColumnSpec ∷ Spec Unit
stringInColumnSpec = describe "stringInColumn" do
  it "centers a string within a column of an even length" do
    liftEffect $ quickCheckGen' 200 $ do
      sideMarginLen ← chooseInt 0 20
      s ← genAsciiString

      let
        sideMargin = joinWith "" (replicate sideMarginLen " ")
        expected = sideMargin <> s <> sideMargin
        actual = stringInColumn (length s + (sideMarginLen * 2)) s

      pure $ toResult { sideMarginLen, s } { actual, expected }

  it "centers a string within a column of an odd length" do
    liftEffect $ quickCheckGen' 200 $ do
      sideMarginLen ← chooseInt 0 20
      s ← genAsciiString

      let
        sideMargin = joinWith "" (replicate sideMarginLen " ")
        expected = sideMargin <> s <> sideMargin <> " "
        actual = stringInColumn (length s + (sideMarginLen * 2 + 1)) s

      pure $ toResult { sideMarginLen, s } { actual, expected }
