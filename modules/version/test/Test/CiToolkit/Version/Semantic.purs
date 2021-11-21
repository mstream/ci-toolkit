module Test.CiToolkit.Version.Semantic (spec) where

import Prelude

import CiToolkit.Common.CI (Repo(Repo))
import CiToolkit.Common.Git.Commit (unsafeCommitRef)
import CiToolkit.Version.Semantic (showSemanticVersion)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Semantic" do
  describe "showSemanticVersion" do
    it "calculates version properly" do
      let
        commitRef = unsafeCommitRef
          "b68df892a0d8571d1d1c4618be5d36641f4a5d9b"

        repo = Repo mempty
        expected = pure "SemanticVersionTODO"

        actual = showSemanticVersion repo commitRef

      actual `shouldEqual` expected
