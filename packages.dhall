let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.14.5-20220204-2/src/packages.dhall sha256:bf284d597ad053b43591b964a52aa0f41ed12a576c3efde85ba999ad65072fc9

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
