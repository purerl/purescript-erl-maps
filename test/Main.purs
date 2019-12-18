module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Data.Map as M
import Test.Assert (assertEqual)

newtype TestMap k v = TestMap (M.Map k v)

main :: Effect Unit
main = do
  assertEqual { actual: M.lookup 42 (M.insert 42 "abc" M.empty)
              , expected: Just "abc"
              }

  assertEqual { actual: M.lookup 1 (M.insert 42 "abc" M.empty)
              , expected: Nothing
              }

  assertEqual { actual: M.values $ M.insert "abc" 1 $ M.insert "def" 2 $ M.empty
              , expected: 1 : 2 : nil
              }
