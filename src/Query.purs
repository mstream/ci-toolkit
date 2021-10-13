module Query (findCommit, findLastCommit) where

import Prelude

import Data.Maybe (Maybe)
import Data.List (List, filter, head, intersect)
import Data.Tuple.Nested (type (/\), (/\))
import CI (CIStage, Repo(Repo))
import Git.Commit (CommitInfo(..), CommitRef)

findCommit ∷ CommitRef → Repo → Maybe (List CIStage /\ CommitInfo)
findCommit commitRef (Repo commits) = do
  found ← head $ filter (\({ ref }) → ref == commitRef) commits
  pure $ found.passedStages /\ found.info

findLastCommit ∷ List CIStage → Repo → Maybe (CommitRef /\ CommitInfo)
findLastCommit requestedPassedStages (Repo commits) = do
  let
    passedRequestedStages { passedStages } =
      intersect passedStages requestedPassedStages ==
        requestedPassedStages

  found ← head $ filter passedRequestedStages commits
  pure $ found.ref /\ found.info
