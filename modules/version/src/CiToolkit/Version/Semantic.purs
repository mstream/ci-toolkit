module CiToolkit.Version.Semantic (showSemanticVersion) where

import Prelude

import CiToolkit.Common.CI (Repo(..), loadRepo)
import CiToolkit.Common.Git (getHeadRef)
import CiToolkit.Common.Git.Commit (CommitRef, commitDateTime)
import CiToolkit.Common.ProgramInput
  ( CommonOptions(CommonOptions)
  , ProgramInput(ProgramInput)
  )
import CiToolkit.Common.ProgramOutput (ProgramOutput(TextOutput))
import CiToolkit.Version.Command (Command(Show))
import CiToolkit.Version.Command.Show
  ( ShowOptions(ShowOptions)
  , VersionFormat(Calendar, Semantic)
  )
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (encodeJson)
import Data.Date (Date, day, month, year)
import Data.DateTime (date)
import Data.DotLang (toGraph)
import Data.Either (Either, either, note)
import Data.Enum (fromEnum)
import Data.Foldable (foldM)
import Data.List (List(Nil), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.String.NonEmpty as NES
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Node.Path (FilePath)

showSemanticVersion ∷ Repo → CommitRef → Either String String
showSemanticVersion repo headRef = pure "SemanticVersionTODO"
