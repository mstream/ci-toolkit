module CiToolkit.Version.Semantic (showSemanticVersion) where

import Prelude

import CiToolkit.Common.CI (Repo)
import CiToolkit.Common.Git.Commit (CommitRef)
import Data.Either (Either)

showSemanticVersion ∷ Repo → CommitRef → Either String String
showSemanticVersion repo headRef = pure "SemanticVersionTODO"
