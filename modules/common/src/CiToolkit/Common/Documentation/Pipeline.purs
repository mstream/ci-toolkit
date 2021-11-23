module CiToolkit.Common.Documentation.Pipeline (commandSuiteInfo) where

import Prelude

import CiToolkit.Common.Documentation
  ( CodeSnippet(CodeSnippet)
  , CommandOption(CommandOption)
  , CommandSuiteInfo(CommandSuiteInfo)
  , HowTo(HowTo)
  , gitDirectoryOption
  )
import CiToolkit.Common.Documentation.Pipeline.GetLast as GetLast
import CiToolkit.Common.Documentation.Pipeline.MarkCommit as MarkCommit
import Data.Maybe (Maybe(Nothing))

commandSuiteInfo ∷ CommandSuiteInfo
commandSuiteInfo = CommandSuiteInfo
  { commands: [ GetLast.commandInfo, MarkCommit.commandInfo ]
  , description:
      [ "The intention of this suite of commands is to keep the track the progress of CI pipelines inside a Git repository instead of CI servers."
      , "In fact, this solution eliminates the necessity of using stateful CI servers at all as each steps of pipelines can be executed by independent, continusously running processes."
      ]
  , howTos:
      [ HowTo
          { codeSnippets:
              [ CodeSnippet
                  { code:
                      [ "# clonning the repository"
                      , "git clone git@github.com:mstream/ci-toolkit.git"
                      , ""
                      , "# running unit tests"
                      , "./run-unit-tests.sh"
                      , ""
                      , "# marking the commit"
                      , "npx @ci-toolkit/pipeline mark-commit \\"
                      , "    --ci-stage unit-testing \\"
                      , "    --commit-ref $(git rev-parse HEAD)"
                      , ""
                      , "# pushing notes"
                      , "git push origin refs/notes/*"
                      ]
                  , title: pure "Process A"
                  }
              , CodeSnippet
                  { code:
                      [ "# clonning the repository"
                      , "git clone git@github.com:mstream/ci-toolkit.git"
                      , ""
                      , "# checking out the last commit which passed unit tests"
                      , "git checkout $(npx @ci-toolkit/pipeline get-last --ci-stage unit-testing)"
                      , ""
                      , "# running functional tests"
                      , "./run-functional-tests.sh"
                      , ""
                      , "# marking the commit"
                      , "npx @ci-toolkit/pipeline mark-commit \\"
                      , "    --ci-stage functional-testing \\"
                      , "    --commit-ref $(git rev-parse HEAD)"
                      , ""
                      , "# pushing notes"
                      , "git push origin refs/notes/*"
                      ]
                  , title: pure "Process B"
                  }
              , CodeSnippet
                  { code:
                      [ "# clonning the repository"
                      , "git clone git@github.com:mstream/ci-toolkit.git"
                      , ""
                      , "# checking out the last commit which passed both unit and functional tests"
                      , "git checkout $(npx @ci-toolkit/pipeline get-last --ci-stage unit-testing --ci-stage functional-testing)"
                      , ""
                      , "# running integration tests"
                      , "./run-integration-tests.sh"
                      , ""
                      , "# marking the commit"
                      , "npx @ci-toolkit/pipeline mark-commit \\"
                      , "    --ci-stage integration-testing \\"
                      , "    --commit-ref $(git rev-parse HEAD)"
                      , ""
                      , "# pushing notes"
                      , "git push origin refs/notes/*"
                      ]
                  , title: pure "Process C"
                  }
              ]
          , title:
              "set up a workflow consisting of three CI stages"
          }
      ]
  , name: "pipeline"
  }
