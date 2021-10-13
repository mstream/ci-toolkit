module CI
  ( CIStage(..)
  , CIStagePrefix(..)
  , Repo(..)
  , ciPrefixParser
  , ciStageParser
  , loadRepo
  ) where

import Prelude
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq.Generic
  ( genericEq
  )
import Data.Generic.Rep
  ( class Generic
  )
import Data.Foldable (foldMap)
import Data.List (List(Nil), singleton)
import Data.Maybe (fromMaybe, maybe)
import Data.Show.Generic
  ( genericShow
  )
import Data.String (Pattern(Pattern), stripPrefix)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Git (getCommitInfo, getCommitNotes, getCommitRefs)
import Git.Commit (CommitInfo, CommitRef, Notes(Notes))
import Node.Path (FilePath)
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (regex)

newtype CIStage = CIStage NonEmptyString

derive instance Generic CIStage _

instance Show CIStage where
  show = genericShow

instance EncodeJson CIStage where
  encodeJson = genericEncodeJson

instance Eq CIStage where
  eq = genericEq

newtype CIStagePrefix = CIStagePrefix NonEmptyString

derive instance Generic CIStagePrefix _

instance Show CIStagePrefix where
  show = genericShow

instance Eq CIStagePrefix where
  eq = genericEq

newtype Repo = Repo
  ( List
      { info ∷ CommitInfo
      , passedStages ∷ List CIStage
      , ref ∷ CommitRef
      }
  )

derive instance Generic Repo _

instance Show Repo where
  show = genericShow

instance EncodeJson Repo where
  encodeJson = genericEncodeJson

instance Eq Repo where
  eq = genericEq

wordParser ∷ Parser NonEmptyString
wordParser = do
  word ← regex "[^ ]+"
  maybe
    (fail "empty word")
    pure
    (NES.fromString word)

ciPrefixParser ∷ Parser CIStagePrefix
ciPrefixParser = do
  ciPrefixName ← wordParser
  pure $ CIStagePrefix ciPrefixName

ciStageParser ∷ Parser CIStage
ciStageParser = do
  ciStageName ← wordParser
  pure $ CIStage ciStageName

passedStagesFromNotes ∷ CIStagePrefix → Notes → List CIStage
passedStagesFromNotes (CIStagePrefix prefix) (Notes noteLines) =
  foldMap (maybe Nil singleton <<< lineToCIStage) noteLines
  where
  lineToCIStage line = do
    suffix ← stripPrefix (Pattern $ NES.toString prefix) line
    ciStageName ← NES.fromString suffix
    pure $ CIStage ciStageName

loadRepo ∷ FilePath → CIStagePrefix → Aff Repo
loadRepo gitDirectory ciPrefix = do
  commitRefs ← getCommitRefs gitDirectory
  commitRefsWithNotes ← traverse
    ( \ref → getCommitNotes gitDirectory ref >>= \notes →
        pure { ref, notes }
    )
    commitRefs
  commitRefsWithNotesAndInfo ← traverse
    ( \commit → do
        commitInfo ← getCommitInfo gitDirectory commit.ref
        pure
          { info: commitInfo
          , passedStages: fromMaybe
              Nil
              (passedStagesFromNotes ciPrefix <$> commit.notes)
          , ref: commit.ref
          }
    )
    commitRefsWithNotes
  pure $ Repo commitRefsWithNotesAndInfo
