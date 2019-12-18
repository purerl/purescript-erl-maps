module Erl.Data.Map (
  module Erl.Data.Map.Internal
) where

import Erl.Data.Map.Internal
  ( Map
  , alter
  , delete
  , difference
  , empty
  , filter
  , filterKeys
  , filterWithKey
  , fromFoldable
  , fromFoldableWith
  , fromFoldableWithIndex
  , insert
  , isEmpty
  , isSubmap
  , keys
  , lookup
  , mapWithKey
  , member
  , singleton
  , size
  , toUnfoldable
  , toUnfoldableUnordered
  , union
  , unionWith
  , unions
  , update
  , values
  )
