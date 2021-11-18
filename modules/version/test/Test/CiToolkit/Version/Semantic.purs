module Test.CiToolkit.Version.Semantic (spec) where

import Prelude

import CiToolkit.Common.CI (CIStage(CIStage), Repo(Repo))
import CiToolkit.Common.CI (CIStagePrefix(CIStagePrefix))
import CiToolkit.Common.Git.Commit (unsafeCommitRef)
import CiToolkit.Common.ProgramInput
  ( CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
  )
import CiToolkit.Common.ProgramOutput
  ( OutputFormat(Text)
  , ProgramOutput(TextOutput)
  )
import CiToolkit.Common.Utils (unsafeNonEmptyString)
import CiToolkit.Version.Program (execute)
import CiToolkit.Version.ProgramInput
  ( Command(Show)
  , ShowOptions(ShowOptions)
  , VersionFormat(Calendar)
  )
import CiToolkit.Version.Semantic (showSemanticVersion)
import Data.Date (Date)
import Data.Date as Date
import Data.DateTime (DateTime(DateTime))
import Data.Enum (toEnum)
import Data.List (List(Nil))
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Time (Time(Time))
import Partial.Unsafe (unsafePartial)
import Test.CiToolkit.Common.TestUtils (createCommit, withGitRepo)
import Test.Spec (Spec, around, describe, it)
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
