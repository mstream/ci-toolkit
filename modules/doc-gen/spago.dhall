{ name = "ci-toolkit-docs-gen"
, dependencies =
  [ "aff"
  , "ci-toolkit-common"
  , "effect"
  , "foldable-traversable"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "optparse"
  , "prelude"
  , "psci-support"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
