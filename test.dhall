let conf = ./spago.dhall

in    conf
    ⫽ { sources =
          [ "src/**/*.purs", "test/**/*.purs" ]
      , dependencies =
          conf.dependencies # [ "assert" ]
      }
