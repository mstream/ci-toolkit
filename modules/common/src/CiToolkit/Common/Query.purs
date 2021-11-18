module CiToolkit.Common.Query
  ( findCommit
  , findLastCommit
  , sortCommitsByTimestamp
  ) where

import Prelude

import CiToolkit.Common.CI (CIStage, Repo(Repo))
import CiToolkit.Common.Git.Commit
  ( CommitInfo
  , CommitRef
  , commitDateTime
  )
import Data.List (List, filter, head, intersect, sortBy)
import Data.Maybe (Maybe)
import Data.Ord (compare)
import Data.Tuple.Nested (type (/\), (/\))

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

sortCommitsByTimestamp ∷ Repo → Repo
sortCommitsByTimestamp (Repo commits) =
  Repo $ sortBy orderByTimestamp commits
  where
  orderByTimestamp { info: info1 } { info: info2 } =
    compare (commitDateTime info1) (commitDateTime info2)
