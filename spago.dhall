{-
Welcome to a Spago project!
You can edit this file as you like.
-}

{ name =
    "purescript-erl-lists"
, backend =
    "purerl"
, dependencies =
    [ "assert"
    , "console"
    , "erl-lists"
    , "functions"
    , "prelude"
    , "tuples"
    , "unfoldable"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
