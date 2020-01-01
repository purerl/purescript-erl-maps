{ name =
    "purescript-erl-maps"
, backend =
    "purerl"
, dependencies =
    [ "console", "erl-lists", "functions", "prelude", "tuples", "unfoldable" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
