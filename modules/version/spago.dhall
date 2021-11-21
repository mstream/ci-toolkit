{ name = "ci-toolkit-version"
, dependencies =
  [ "aff"
  , "argonaut"
  , "ci-toolkit-common"
  , "datetime"
  , "dotlang"
  , "effect"
  , "either"
  , "exceptions"
  , "enums"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "node-path"
  , "optparse"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "spec"
  , "string-parsers"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
