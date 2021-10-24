module CiToolkit.Common.CI
  ( CIStage(..)
  , CIStagePrefix(..)
  , RenderRepoOpts(..)
  , Repo(..)
  , ciPrefixParser
  , ciStageParser
  , loadRepo
  ) where

import Prelude

import CiToolkit.Common.Git
  ( getCommitInfo
  , getCommitNotes
  , getCommitRefs
  , getTagInfo
  , getTags
  )
import CiToolkit.Common.Git.Commit
  ( CommitInfo(CommitInfo)
  , CommitParent(CommitParent)
  , CommitRef
  , GitObjectRefFormat(FullHex, ShortHex)
  , Notes(Notes)
  )
import CiToolkit.Common.Git.Parsing (wordParser)
import CiToolkit.Common.Git.Tag (Tag, getTagCommitRef)
import CiToolkit.Common.Print (stringInColumn)
import CiToolkit.Common.Text.SerDe (class Serializable, serialize)
import Color.Scheme.MaterialDesign (blueGrey, yellow)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (fromFoldable)
import Data.DotLang
  ( class GraphRepr
  , Definition
  , Graph(DiGraph)
  , node
  , (==>)
  )
import Data.DotLang.Attr.Node
  ( Attr(FillColor, Shape)
  , ShapeType(Oval, Record)
  , label
  , recordLabel
  , subLabel
  )
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
  , zip
  , (:)
  )
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Show.Generic
  ( genericShow
  )
import Data.String
  ( Pattern(Pattern)
  , Replacement(Replacement)
  , length
  , replaceAll
  , stripPrefix
  )
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Prettier.Printer
  ( below
  , beside
  , folddoc
  , pretty
  , text
  , (<+>)
  , (</>)
  )
import Text.Parsing.StringParser (Parser)

newtype RenderRepoOpts = RenderRepoOpts
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
      , tags ∷ List Tag
      }
  )

derive instance Generic Repo _

instance Show Repo where
  show = genericShow

instance EncodeJson Repo where
  encodeJson = genericEncodeJson

instance GraphRepr Repo where
  toGraph = repoToGraph

instance Eq Repo where
  eq = genericEq

instance Serializable Repo RenderRepoOpts where
  serialize = printRepo

sanitizeNodeId ∷ String → String
sanitizeNodeId =
  replaceAll (Pattern ".") (Replacement "_")
    <<< replaceAll (Pattern "-") (Replacement "_")

sanitizeCommitMessage ∷ String → String
sanitizeCommitMessage msg =
  if length msg > 97 then String.take 97 msg <> "..." else msg

sanitizeRecordLabel ∷ String → String
sanitizeRecordLabel =
  replaceAll (Pattern "|") (Replacement " ")
    <<< replaceAll (Pattern "{") (Replacement " ")
    <<< replaceAll (Pattern "}") (Replacement " ")

commitRefToNodeId ∷ CommitRef → String
commitRefToNodeId commitRef = sanitizeNodeId $ "commit_" <> serialize
  FullHex
  commitRef

tagToNodeId ∷ Tag → String
tagToNodeId tag = sanitizeNodeId $ "tag_" <> serialize unit tag

commitToNodes
  ∷ ∀ r
  . { info ∷ CommitInfo, ref ∷ CommitRef, tags ∷ List Tag | r }
  → Array Definition
commitToNodes { info, ref, tags } =
  let
    (CommitInfo { message }) = info
  in
    [ node
        (commitRefToNodeId ref)
        [ FillColor yellow
        , Shape Record
        , recordLabel
            [ subLabel $ sanitizeRecordLabel $ serialize ShortHex ref
            , subLabel $ sanitizeRecordLabel $ sanitizeCommitMessage $
                serialize unit message
            ]
        ]
    ] <> (fromFoldable $ tagToNode <$> tags)

tagToNode
  ∷ Tag → Definition
tagToNode tag =
  node
    (tagToNodeId tag)
    [ FillColor blueGrey
    , Shape Oval
    , label $ serialize unit tag
    ]

commitToEdges
  ∷ ∀ r
  . { info ∷ CommitInfo, ref ∷ CommitRef, tags ∷ List Tag | r }
  → Array Definition
commitToEdges { info, ref, tags } =
  let
    (CommitInfo { parents }) = info
  in
    fromFoldable $
      ( tags <#> \tag →
          tagToNodeId tag ==> commitRefToNodeId ref
      ) <>
        ( parents <#> \(CommitParent parentRef) →
            commitRefToNodeId ref ==> commitRefToNodeId parentRef
        )

repoToGraph ∷ Repo → Graph
repoToGraph (Repo commits) =
  DiGraph $ (foldMap commitToNodes commits) <>
    (foldMap commitToEdges commits)

printRepo ∷ RenderRepoOpts → Repo → String
printRepo
  (RenderRepoOpts { ciStagesOrder, commitsLimit })
  (Repo commits) =
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
      <<< serialize FullHex

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
  tagAndInfos ← do
    tags ← getTags gitDirectory
    infos ← traverse (getTagInfo gitDirectory) tags
    pure $ zip tags infos

  commits ← traverse
    ( \commitRef → do
        commitInfo ← getCommitInfo gitDirectory commitRef
        notes ← getCommitNotes gitDirectory commitRef

        let
          commitTags = foldMap
            ( \(tag /\ tagInfo) →
                if getTagCommitRef tagInfo == commitRef then tag : Nil
                else Nil
            )
            tagAndInfos

        pure
          { info: commitInfo
          , passedStages: fromMaybe
              Nil
              (passedStagesFromNotes ciPrefix <$> notes)
          , ref: commitRef
          , tags: commitTags
          }
    )
    commitRefs

  pure $ Repo commits
