let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210919/packages.dhall
        sha256:03516fdd4f6d1bd6c9eb5e63cf3af3037bc079459117ab93eb85b6eb46e258a7

in  upstream
  with node-os =
    { dependencies = [ "datetime", "foreign-object", "node-buffer", "prelude" ]
    , repo = "https://github.com/Thimoteus/purescript-node-os"
    , version = "v3.1.0"
    }
  with dotlang =
    { dependencies =
      [ "colors", "console", "effect", "prelude", "psci-support", "strings" ]
    , repo = "https://github.com/csicar/purescript-dotlang"
    , version = "4d7bedc91c8510f102188c97b0c50b475b0beb64"
    }
