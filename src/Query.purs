module Query (findLastCommit) where

import Prelude

import Data.Maybe (Maybe)
import Data.List (List, filter, head, intersect)
import Data.Tuple.Nested (type (/\), (/\))
import CI (CIStage, Repo(Repo))
import Git.Commit (CommitInfo, CommitRef)

findLastCommit ∷ List CIStage → Repo → Maybe (CommitRef /\ CommitInfo)
findLastCommit requestedPassedStages (Repo commits) = do
  let
    passedRequestedStages { passedStages } =
      intersect passedStages requestedPassedStages ==
        requestedPassedStages

  found ← head $ filter passedRequestedStages commits
  pure $ found.ref /\ found.info
