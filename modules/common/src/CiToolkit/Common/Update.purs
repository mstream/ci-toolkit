module CiToolkit.Common.Update (Update(..), markCommit) where

import Prelude

import CiToolkit.Common.CI (CIStage(..), Repo)
import CiToolkit.Common.Git.Commit
  ( CommitRef
  , GitObjectRefFormat(FullHex)
  )
import CiToolkit.Common.Query (findCommit)
import CiToolkit.Common.Text.SerDe (class Serializable, serialize)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (elem)
import Data.Maybe (Maybe(Nothing))
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))

data Update =
  MarkWithCIStage CommitRef CIStage

derive instance Generic Update _

instance Show Update where
  show = genericShow

instance EncodeJson Update where
  encodeJson = genericEncodeJson

instance Eq Update where
  eq = genericEq

instance Serializable Update Unit where
  serialize _ = case _ of
    MarkWithCIStage commitRef (CIStage stage) →
      "Marking commit "
        <> (serialize FullHex commitRef)
        <> " with CI stage '"
        <> NES.toString stage
        <> "'"

markCommit ∷ CIStage → CommitRef → Repo → Maybe Update
markCommit ciStage commitRef repo = do
  passedStages /\ _ ← findCommit commitRef repo
  if elem ciStage passedStages then Nothing
  else pure $ MarkWithCIStage commitRef ciStage
