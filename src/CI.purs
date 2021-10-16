module CI
  ( CIStage(..)
  , CIStagePrefix(..)
  , PrintRepoOpts(..)
  , Repo(..)
  , ciPrefixParser
  , ciStageParser
  , loadRepo
  ) where

import Prelude
import Prettier.Printer
  ( (<+>)
  , (</>)
  , below
  , beside
  , folddoc
  , pretty
  , text
  )
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq.Generic
  ( genericEq
  )
import Data.Foldable (elem, foldMap, foldr)
import Data.Generic.Rep
  ( class Generic
  )
import Data.List
  ( List(Nil)
  , concatMap
  , elemIndex
  , nubEq
  , singleton
  , sortBy
  , take
  )
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
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
import Print (class Printable, showToHuman, stringInColumn)
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (regex)

newtype PrintRepoOpts = PrintRepoOpts
  { ciStagesOrder ∷ List CIStage, commitsLimit ∷ Int }

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

instance Printable Repo PrintRepoOpts where
  showToHuman = printRepo

printRepo ∷ PrintRepoOpts → Repo → String
printRepo (PrintRepoOpts { ciStagesOrder, commitsLimit }) (Repo commits) =
  let
    stagesComparator a b =
      case elemIndex a ciStagesOrder, elemIndex b ciStagesOrder of
        Nothing, Nothing → EQ
        Just _, Nothing → LT
        Nothing, Just _ → GT
        Just i, Just j
          | i < j → LT
          | i > j → GT
          | otherwise → EQ

    allStages = sortBy stagesComparator $ nubEq $ concatMap
      (_.passedStages)
      commits

    stringInCommitRefColumn = stringInColumn 40

    stringInStageColumn = stringInColumn $ foldr
      (\(CIStage stage) → \maxLen → max maxLen (NES.length stage))
      0
      allStages

    heading =
      (text $ stringInCommitRefColumn "Commit ID") <+>
        (stagesToDoc allStages)

    commitToDoc { passedStages, ref } = commitRefToDoc ref <+>
      stagesToTickDoc passedStages

    commitRefToDoc = text
      <<< stringInCommitRefColumn
      <<< showToHuman unit

    stagesToDoc = folddoc beside <<< map stageToDoc

    stageToDoc (CIStage stageName) =
      text $ stringInStageColumn $ NES.toString stageName

    stagesToTickDoc stages = folddoc
      beside
      ( allStages <#> \stage → text $ stringInStageColumn $
          if elem stage stages then "+" else "-"
      )
  in
    pretty 80
      ( heading </>
          ( folddoc below
              ( commitToDoc <$>
                  ( if commitsLimit > 0 then take commitsLimit
                    else identity
                  ) commits
              )
          )
      )

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
