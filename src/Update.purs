module Update (Update(..), markCommit) where

import Prelude

import CI (CIStage(..), Repo)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq.Generic (genericEq)
import Data.List (elem)
import Data.Maybe (Maybe(Nothing))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Git.Commit (CommitRef, asHex)
import Print (class Printable)
import Query (findCommit)

data Update =
  MarkWithCIStage CommitRef CIStage

derive instance Generic Update _

instance Show Update where
  show = genericShow

instance EncodeJson Update where
  encodeJson = genericEncodeJson

instance Eq Update where
  eq = genericEq

instance Printable Update where
  showToHuman = case _ of
    MarkWithCIStage commitRef (CIStage stage) →
      "Marking commit "
        <> asHex commitRef
        <> " with CI stage '"
        <> NES.toString stage
        <> "'"

markCommit ∷ CIStage → CommitRef → Repo → Maybe Update
markCommit ciStage commitRef repo = do
  passedStages /\ _ ← findCommit commitRef repo
  if elem ciStage passedStages then Nothing
  else pure $ MarkWithCIStage commitRef ciStage
