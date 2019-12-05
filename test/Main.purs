module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Data.Map as Map
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  assertEqual { actual: Map.lookup 42 (Map.insert 42 "abc" Map.empty)
              , expected: Just "abc"
              }

  assertEqual { actual: Map.lookup 1 (Map.insert 42 "abc" Map.empty)
              , expected: Nothing
              }

  assertEqual { actual: Map.values $ Map.insert "abc" 1 $ Map.insert "def" 2 $ Map.empty
              , expected: 1 : 2 : nil
              }