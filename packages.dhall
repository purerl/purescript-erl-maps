let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.13.5-20191204/src/packages.dhall sha256:308bc2dd2c5700e6e22fae85b10472fd76ddaaf8034711ff0eaede145573479a

let overrides =
      { erl-test-eunit =
          { dependencies =
              [ "assert"
              , "console"
              , "debug"
              , "erl-lists"
              , "erl-tuples"
              , "foreign"
              , "free"
              , "prelude"
              , "psci-support"
              ]
          , repo =
              "ssh://git@github.com/id3as/purescript-erl-test-eunit.git"
          , version =
              "ae8e2573851a70eb44199ae1eef7dcb8537184d5"
          }
      }

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
