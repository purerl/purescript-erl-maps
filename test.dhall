let conf = ./spago.dhall

in    conf
    ⫽ { sources =
          conf.sources # [ "test/**/*.purs" ]
      , dependencies =
          conf.dependencies # [ "assert", "erl-test-eunit", "arrays", "control", "effect", "foldable-traversable", "maybe"]
      }
