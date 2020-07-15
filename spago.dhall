{ name = "purescript-erl-maps"
, backend = "purerl"
, dependencies = [ "erl-lists", "functions", "prelude", "tuples", "unfoldable" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
