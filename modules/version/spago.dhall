{ name = "ci-toolkit-version"
, dependencies =
  [ "aff"
  , "argonaut"
  , "ci-toolkit-common"
  , "dotlang"
  , "effect"
  , "either"
  , "lists"
  , "maybe"
  , "node-path"
  , "optparse"
  , "prelude"
  , "psci-support"
  , "spec"
  , "string-parsers"
  , "strings"
  , "tuples"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
