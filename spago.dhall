{ name = "purescript-erl-maps"
, backend = "purerl"
, dependencies =
  [ "either"
  , "erl-lists"
  , "filterable"
  , "functions"
  , "newtype"
  , "prelude"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
