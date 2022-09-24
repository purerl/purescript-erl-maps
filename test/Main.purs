module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array as A
import Data.Foldable (and)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, uncurry)
import Effect (Effect)
import Erl.Data.List (List, length, nil, nubBy, singleton, sort, sortBy, (:))
import Erl.Data.Map as M
import Erl.Test.EUnit (runTests, suite, test)
import Test.Assert (assert, assertEqual)

main :: Effect Unit
main =
  void $ runTests do
    suite "Test Erl.Data.Map" do
      test "Test inserting into empty Map" do
        assertEqual { actual: M.lookup 42 (M.insert 42 "abc" M.empty)
                    , expected: Just "abc"
                    }

      test "Test inserting two values with same key" do
        assertEqual { actual: M.lookup 42 (M.insert 42 "abc" (M.insert 42 "def" M.empty))
                    , expected: Just "abc"
                    }

      test "Test insertWith combining values" do
        assertEqual { actual: M.lookup 1 (M.insertWith (+) 1 42 (M.insert 1 43 M.empty))
                    , expected: Just (42 + 43)
                    }

      test "Test insertWith passes the first value as the first argument to the combining function" do
        assertEqual { actual: M.lookup 1 (M.insertWith const 1 "abc" (M.insert 1 "def" M.empty))
                    , expected: Just "def"
                    }

      test "Test delete after inserting" do
        assertEqual { actual: M.lookup 1 (M.delete 1 (M.insert 1 "abc" M.empty))
                    , expected: Nothing
                    }

      test "Insert two, lookup first" do
        assertEqual { actual: M.lookup 1 (M.insert 2 "def" (M.insert 1 "abc" M.empty))
                    , expected: Just "abc"
                    }

      test "Insert two, lookup second" do
        assertEqual { actual: M.lookup 2 (M.insert 2 "def" (M.insert 1 "abc" M.empty))
                    , expected: Just "def"
                    }

      test "Insert two, delete one" do
        assertEqual { actual: M.lookup 2 (M.delete 1 (M.insert 2 "def" (M.insert 1 "abc" M.empty)))
                    , expected: Just "def"
                    }

      test "Lookup from empty" do
        assertEqual { actual: M.lookup 1 (M.empty :: M.Map Int String)
                    , expected: Nothing
                    }

      test "Lookup from singleton" do
        assertEqual { actual: M.lookup 1 (M.singleton 1 "abc")
                    , expected: Just "abc"
                    }

      test "Singleton to list" do
        assertEqual { actual: M.toUnfoldable (M.singleton 1 "abc")
                    , expected: singleton (Tuple 1 "abc")
                    }

      test "fromFoldable & key collision" do
        let nums = M.fromFoldable [Tuple 0 "zero", Tuple 1 "what", Tuple 1 "one"]
        assertEqual { actual: M.lookup 0 nums, expected: Just "zero" }
        assertEqual { actual: M.lookup 1 nums, expected: Just "one" }
        assertEqual { actual: M.lookup 2 nums, expected: Nothing }

      test "fromFoldableWith const [] = empty" do
        assertEqual { actual: M.fromFoldableWith const []
                    , expected: M.empty :: M.Map Unit Unit
                    }

      test "fromFoldableWith (+) & key collision" do
        let nums = M.fromFoldableWith (+) [Tuple 0 1, Tuple 1 1, Tuple 1 1]
        assertEqual { actual: M.lookup 0 nums, expected: Just 1 }
        assertEqual { actual: M.lookup 1 nums, expected: Just 2 }
        assertEqual { actual: M.lookup 2 nums, expected: Nothing }

      test "sort . toUnfoldable . fromFoldable = sort (on lists without key-duplicates)" do
        let nubbedList = nubBy ((==) `on` fst) ((Tuple 1 41) : (Tuple 2 42) : nil) :: List (Tuple Int Int)
            f x = M.toUnfoldable (M.fromFoldable x)
        assertEqual { actual: sort (f nubbedList)
                    , expected: sort nubbedList
                    }

      test "fromFoldable . toUnfoldable = id" do
        let m    = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            f m' = M.fromFoldable (M.toUnfoldable m' :: List (Tuple Int Int))
        assertEqual { actual: f m, expected: m }

      test "fromFoldableWith const = fromFoldable" do
        assertEqual { actual: M.fromFoldableWith const [Tuple 0 0]
                    , expected: M.fromFoldable [Tuple 0 0]
                    }

      test "toUnfoldable is sorted" do
        let m       = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            list    = M.toUnfoldable m
            ascList = M.toUnfoldable m
        assertEqual { actual: ascList
                    , expected: sortBy (compare `on` fst) list
                    }

      test "Lookup from union" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            m2 = M.fromFoldable [Tuple 3 3, Tuple 4 4, Tuple 5 5]
            result = case M.lookup 1 m1 of
                       Nothing ->  M.lookup 1 m2
                       Just v  -> Just v
        assertEqual { actual: M.lookup 1 (M.union m1 m2)
                    , expected: result
                    }

      test "Union is idempotent" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            m2 = M.fromFoldable [Tuple 3 3, Tuple 1 0, Tuple 5 5]
        assertEqual { actual: (m1 `M.union` m2)
                    , expected: (m1 `M.union` m2) `M.union` m2
                    }

      test "Union prefers left" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            m2 = M.fromFoldable [Tuple 3 3, Tuple 1 1, Tuple 5 5]
        assertEqual { actual: M.lookup 1 (M.union m1 m2)
                    , expected: (M.lookup 1 m1 <|> M.lookup 1 m2)
                    }

      test "UnionWith uses merge function" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            m2 = M.fromFoldable [Tuple 1 11, Tuple 2 12, Tuple 4 14]
        assertEqual { actual: M.unionWith (\a b -> a * 100 + b) m1 m2
                    , expected: M.fromFoldable [Tuple 0 0, Tuple 1 111, Tuple 2 212, Tuple 4 14]
                    }

      test "Unions prefers first" do
        let m1 = M.fromFoldable [Tuple 1 1, Tuple 5 1]
            m2 = M.fromFoldable [Tuple 2 2, Tuple 5 2]
            m3 = M.fromFoldable [Tuple 3 3, Tuple 5 3]
        assertEqual { actual: M.unions [m1, m2, m3]
                    , expected: M.fromFoldable [Tuple 1 1, Tuple 2 2, Tuple 3 3, Tuple 5 3]
                    }
        assertEqual { actual: M.unions [m3, m2, m1]
                    , expected: M.fromFoldable [Tuple 1 1, Tuple 2 2, Tuple 3 3, Tuple 5 1]
                    }
        assertEqual { actual: M.unions [m3, m1, m2]
                    , expected: M.fromFoldable [Tuple 1 1, Tuple 2 2, Tuple 3 3, Tuple 5 2]
                    }
        assertEqual { actual: M.unions [m1, m2]
                    , expected: M.fromFoldable [Tuple 1 1, Tuple 2 2, Tuple 5 2]
                    }

      -- alt is just union
      test "Alt is idempotent" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            m2 = M.fromFoldable [Tuple 3 3, Tuple 1 0, Tuple 5 5]
        assertEqual { actual: (m1 <|> m2)
                    , expected: m1 <|> m2 <|> m2
                    }

      test "Alt prefers left" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            m2 = M.fromFoldable [Tuple 3 3, Tuple 1 1, Tuple 5 5]
        assertEqual { actual: M.lookup 1 (m1 <|> m2)
                    , expected: (M.lookup 1 m1 <|> M.lookup 1 m2)
                    }

      test "Plus empty is the empty map" do
        assertEqual { actual: empty :: M.Map Int String
                    , expected: M.empty
                    }

      test "Apply" do
        let m1 = M.fromFoldable [Tuple 0 (_ + 10), Tuple 1 (_ + 10), Tuple 2 (_ + 20), Tuple 3 (_ + 20), Tuple 4 (_ + 20)]
            m2 = M.fromFoldable [Tuple 4 4, Tuple 1 1, Tuple 5 5, Tuple 5 5]
            m3 = M.fromFoldable [Tuple 1 11, Tuple 4 24]
        assertEqual { actual: m1 <*> m2
                    , expected: m3
                    }

      test "Bind" do
        let f a = M.fromFoldable [Tuple a (a + 1), Tuple (a + 10) (a + 10)]
            g _ = empty
            h _ = M.fromFoldable [Tuple 4 14, Tuple 5 15, Tuple 6 16]
            m1 = M.fromFoldable [Tuple 1 1, Tuple 5 5]
        assertEqual { actual: m1 >>= f
                    , expected: M.fromFoldable [Tuple 1 2, Tuple 5 6]
                    }
        assertEqual { actual: m1 >>= g
                    , expected: M.empty :: M.Map Int String
                    }
        assertEqual { actual: m1 >>= h
                    , expected: M.fromFoldable [Tuple 5 15]
                    }

      test "Semigroup" do
        let m1 = M.fromFoldable [Tuple 0 "a0", Tuple 1 "a1", Tuple 2 "a2"]
            m2 = M.fromFoldable [Tuple 3 "b3", Tuple 1 "b1", Tuple 5 "b5"]
        assertEqual { actual: m1 <> m2
                    , expected: M.fromFoldable [Tuple 0 "a0", Tuple 1 "a1b1", Tuple 2 "a2", Tuple 3 "b3", Tuple 5 "b5"]
                    }

      test "difference" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            m2 = M.fromFoldable [Tuple 3 3, Tuple 1 1, Tuple 5 5]
            d  = M.difference m1 m2

        assert (and (map (\_ -> M.member 1 m1) (A.fromFoldable $ M.keys d)) &&
                and (map (\_ -> not $ M.member 1 d) (A.fromFoldable $ M.keys m2)))

      test "intersection" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            m2 = M.fromFoldable [Tuple 3 "c", Tuple 1 "a", Tuple 5 "e"]
        assertEqual { actual: M.intersection m1 m2
                    , expected: M.fromFoldable [Tuple 1 1]
                    }

      test "intersectionWith" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            m2 = M.fromFoldable [Tuple 3 30, Tuple 1 10, Tuple 5 50]
        assertEqual { actual: M.intersectionWith (+) m1 m2
                    , expected: M.fromFoldable [Tuple 1 11]
                    }

      test "size" do
        let xs = nubBy ((==) `on` fst) ((Tuple 1 41) : (Tuple 2 42) : nil)
        assertEqual { actual: M.size (M.fromFoldable xs)
                    , expected: length xs
                    }
      test "filterWithKey keeps those keys for which predicate is true" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
        assert (A.all
                 (uncurry (>))
                 (M.toUnfoldable (M.filterWithKey (>) m1) :: Array (Tuple Int Int)))


      test "filterKeys keeps those keys for which predicate is true" do
        let m1 = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
        assert (A.all
                 ((>) 2) (A.fromFoldable (M.keys (M.filterKeys ((>) 2) m1)))
               )

      test "Member" do
        let m1  = M.fromFoldable [Tuple 0 0, Tuple 1 1, Tuple 2 2]
            in1 = M.member 1 m1
            in2 = M.member 3 m1
        assert $ not (in1 == in2)
