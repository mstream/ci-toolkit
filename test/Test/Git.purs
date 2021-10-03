module Test.Git (spec) where

import Prelude

import Data.List (fromFoldable)
import Git (getCommitRefs)
import Git.Commit (unsafeCommitRef)
import Test.Spec (Spec, around, describe, it)
import Test.Utils (createCommit, withGitRepo)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = describe "Git" do
  describe "getCommitRefs" do
    around withGitRepo do
      it "retrieves all commit refs" $ \gitDirPath → do
        createCommit gitDirPath "commit1"
        createCommit gitDirPath "commit2"
        let
          expected = fromFoldable
            [ unsafeCommitRef "e2d19a458ede8d91d85cd9be71c4e71b84fdf385"
            , unsafeCommitRef "3382801b3e3e771fe2b1b5ea4e42ef4ee7867103"
            ]
        actual ← getCommitRefs gitDirPath
        actual `shouldEqual` expected
