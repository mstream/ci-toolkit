module Main where

import Prelude

import CiToolkit.Common.Documentation
  ( CommandInfo(CommandInfo)
  , CommandSuiteInfo(CommandSuiteInfo)
  , ProjectInfo(ProjectInfo)
  , commandInfoToAsciiDoc
  , commandSuiteInfoToAsciiDoc
  , projectInfoToAsciiDoc
  )
import CiToolkit.Common.Documentation.Project (projectInfo)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (writeTextFile)
import Node.Path (FilePath, sep)
import Options.Applicative ((<**>))
import Options.Applicative as Opts

main ∷ Effect Unit
main = do
  filePath ← Opts.execParser
    ( Opts.info
        (repoDirParser <**> Opts.helper)
        Opts.fullDesc
    )

  launchAff_ $ generate filePath

repoDirParser ∷ Opts.Parser FilePath
repoDirParser = Opts.argument Opts.str (Opts.metavar "PATH")

generate ∷ FilePath → Aff Unit
generate repoDir = do
  let
    filePath name =
      repoDir <> sep <> name

    writeToModule moduleName fileName fileContents =
      writeTextFile
        UTF8
        ( filePath $ "modules"
            <> sep
            <> moduleName
            <> sep
            <> fileName
            <> ".adoc"
        )
        fileContents

    writeSuite
      moduleName
      (commandSuiteInfo@(CommandSuiteInfo { commands })) =
      do
        void $ traverse
          ( \commandInfo@(CommandInfo { name }) →
              writeToModule
                moduleName
                name
                (commandInfoToAsciiDoc commandInfo)
          )
          commands

        writeToModule
          moduleName
          "README"
          (commandSuiteInfoToAsciiDoc commandSuiteInfo)

    writeProject (projectInfo@(ProjectInfo { commandSuites })) = do
      void $ traverse
        ( \commandSuite@(CommandSuiteInfo { name }) →
            writeSuite
              name
              commandSuite
        )
        commandSuites

      writeTextFile
        UTF8
        (filePath "README.adoc")
        (projectInfoToAsciiDoc projectInfo)

  writeProject projectInfo
