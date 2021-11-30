{ name = "ci-toolkit-render"
, dependencies =
  [ "aff"
  , "argonaut"
  , "ci-toolkit-common"
  , "control"
  , "dotlang"
  , "effect"
  , "lists"
  , "node-path"
  , "optparse"
  , "prelude"
  , "psci-support"
  , "spec"
  , "strings"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
