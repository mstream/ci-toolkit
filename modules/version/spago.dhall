{ name = "ci-toolkit-version"
, dependencies =
  [ "aff"
  , "ci-toolkit-common"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "enums"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "optparse"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
