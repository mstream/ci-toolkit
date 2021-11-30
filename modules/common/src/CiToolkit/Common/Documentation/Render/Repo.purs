module CiToolkit.Common.Documentation.Render.Repo
  ( RepoOptions(..)
  , commandInfo
  , repoOptionsParser
  ) where

import Prelude

import CiToolkit.Common.CI (CIStage, CIStagePrefix)
import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandInfo(CommandInfo)
  , CommandOption(CommandOption)
  , HowTo(HowTo)
  , ciStagePrefixOption
  , commandOptionDescription
  , commandOptionToParser
  , gitDirectoryOption
  , outputFormatOption
  )
import CiToolkit.Common.ProgramInput (parseCIStage)
import CiToolkit.Common.ProgramOutput (OutputFormat(DOT, JSON, Text))
import Control.Plus (empty)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Options.Applicative as Opts

newtype RepoOptions = RepoOptions
  { ciStagePrefix ∷ CIStagePrefix
  , ciStages ∷ List CIStage
  , outputFormat ∷ OutputFormat
  }

derive instance Generic RepoOptions _

instance Show RepoOptions where
  show = genericShow

repoOptionsParser ∷ Opts.Parser RepoOptions
repoOptionsParser = ado
  ciStagePrefix ← commandOptionToParser ciStagePrefixOption
  ciStages ← Opts.many $ commandOptionToParser ciStageOption
  outputFormat ← commandOptionToParser outputFormatOption
  in RepoOptions { ciStagePrefix, ciStages, outputFormat }

commandInfo ∷ CommandInfo
commandInfo = CommandInfo
  { description: [ "Renders the entire repository." ]
  , howTos:
      [ HowTo
          { codeSnippets:
              [ CodeSnippet
                  { code:
                      [ "npx @ci-toolkit/render repo \\"
                      , "    --format dot | dot -Tpng > /tmp/output.png"
                      ]
                  , title: mempty
                  }
              ]
          , title: "generate a graphical repository visualization"
          }
      ]
  , name: "repo"
  , options:
      [ commandOptionDescription ciStagePrefixOption
      , commandOptionDescription $ CommandOption
          { defaultValue: pure JSON
          , description:
              [ "Format in which the output should be produced." ]
          , longForm: "format"
          , reader: Opts.eitherReader $ case _ of
              "dot" → pure DOT
              "json" → pure JSON
              "text" → pure Text
              unsupportedFormat → Left $ "unsupported format: " <>
                unsupportedFormat
          , shortForm: empty
          }
      , commandOptionDescription gitDirectoryOption
      ]
  }

ciStageOption ∷ CommandOption CIStage
ciStageOption = CommandOption
  { defaultValue: empty
  , description:
      [ "Order of stages to be render with."
      ]
  , longForm: "ci-stage"
  , reader: parseCIStage
  , shortForm: empty
  }
