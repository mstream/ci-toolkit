{ name = "ci-toolkit-pipeline"
, dependencies =
  [ "aff"
  , "ci-toolkit-common"
  , "control"
  , "effect"
  , "lists"
  , "maybe"
  , "node-path"
  , "optparse"
  , "prelude"
  , "psci-support"
  , "spec"
  , "strings"
  , "tuples"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
