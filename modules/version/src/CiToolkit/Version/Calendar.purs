module CiToolkit.Version.Calendar
  ( formatCalendarVersion
  , showCalendarVersion
  ) where

import Prelude

import CiToolkit.Common.CI (Repo(..))
import CiToolkit.Common.Git.Commit (CommitRef, commitDateTime)
import CiToolkit.Common.Query (sortCommitsByTimestamp)
import Data.Date (Date, day, month, year)
import Data.DateTime (date)
import Data.Either (Either, note)
import Data.Enum (fromEnum)
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))

showCalendarVersion ∷ Repo → CommitRef → Either String String
showCalendarVersion repo headRef = do
  let
    Repo commits = sortCommitsByTimestamp repo
    go { counter, datesByCommitRef, lastDate } { info, ref } = do
      d ← date <$> note "invalid commit date" (commitDateTime info)

      let
        sameDateAsPrevious = maybe false (_ == d) lastDate
        newCounter = if sameDateAsPrevious then counter + 1 else 1

      pure
        { counter: newCounter
        , datesByCommitRef: Map.insert
            ref
            (d /\ newCounter)
            datesByCommitRef
        , lastDate: Just d
        }

  { datesByCommitRef } ← foldM
    go
    { counter: 1, datesByCommitRef: Map.empty, lastDate: Nothing }
    commits

  uncurry formatCalendarVersion <$> note "No commits"
    (Map.lookup headRef datesByCommitRef)

formatCalendarVersion ∷ Date → Int → String
formatCalendarVersion commitDate commitIndex =
  let
    y = fromEnum $ year commitDate
    m = fromEnum $ month commitDate
    d = fromEnum $ day commitDate

    padded x = (if x < 10 then "0" else "") <> show x

  in
    show y
      <> "."
      <> padded m
      <> "."
      <> padded d
      <> "_"
      <> show commitIndex
