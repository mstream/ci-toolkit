{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "formatters"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "node-buffer"
  , "node-child-process"
  , "node-fs-aff"
  , "node-os"
  , "node-path"
  , "nonempty"
  , "numbers"
  , "optparse"
  , "partial"
  , "prelude"
  , "prettier-printer"
  , "psci-support"
  , "quickcheck"
  , "random"
  , "spec"
  , "string-parsers"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
