module CiToolkit.Common.CLI (run) where

import Prelude

import CiToolkit.Common.ProgramOutput (ProgramOutput)
import CiToolkit.Common.Text.SerDe (serialize)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (caseJsonObject, caseJsonString)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(Left), either)
import Data.Maybe (fromMaybe, maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Foreign.Object (lookup)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Globals (__dirname)
import Node.Path (sep)
import Options.Applicative ((<**>))
import Options.Applicative as Opts

run ∷ ∀ i. Opts.Parser i → (String → i → Aff ProgramOutput) → Aff Unit
run programInputParser execute = do
  version ← getVersion
  args ← liftEffect $ Opts.execParser $ options programInputParser
  output ← execute version args
  liftEffect $ log $ serialize unit output

options ∷ ∀ i. Opts.Parser i → Opts.ParserInfo i
options programInputParser = Opts.info
  (programInputParser <**> Opts.helper)
  (Opts.fullDesc)

getVersion ∷ Aff String
getVersion = do
  let
    packageFileName = "package.json"

  packageText ← readTextFile
    UTF8
    (__dirname <> sep <> ".." <> sep <> packageFileName)

  either
    (throwError <<< error)
    pure
    (parseVersion packageFileName packageText)

parseVersion ∷ String → String → Either String String
parseVersion fileName s = do
  json ← jsonParser s
  versionJson ← caseJsonObject
    (Left $ fileName <> "does not contain any JSON object")
    ( \obj →
        maybe
          (Left $ "no version field in " <> fileName)
          pure
          (lookup "version" obj)
    )
    json
  caseJsonString
    (Left $ "version property of " <> fileName <> " is not a string")
    pure
    versionJson
