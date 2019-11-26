module Erl.Data.Map
  ( Map
  , empty
  , isEmpty
  , insert
  , singleton
  , lookup
  , values
  , mapWithKey
  ) where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault)
import Erl.Data.List (List)

foreign import data Map :: Type -> Type -> Type

foreign import empty :: forall a b. Map a b

foreign import isEmpty :: forall a b. Map a b -> Boolean

foreign import insert :: forall a b. a -> b -> Map a b -> Map a b

singleton :: forall a b. a -> b -> Map a b
singleton a b = insert a b empty

foreign import lookupImpl :: forall a b z. z -> (b -> z) -> a -> Map a b -> z

lookup :: forall a b. a -> Map a b -> Maybe b
lookup = lookupImpl Nothing Just

foreign import mapImpl :: forall k a b. (a -> b) -> Map k a -> Map k b

instance functorMap :: Functor (Map a) where
  map f m = mapImpl f m

foreign import mapWithKeyImpl :: forall k a b. (Fn2 k a b) -> Map k a -> Map k b

mapWithKey :: forall k a b. (k -> a -> b) -> Map k a -> Map k b
mapWithKey f m = mapWithKeyImpl (mkFn2 f) m

foreign import values :: forall a b. Map a b -> List b

-- Folds taken from purescript-foreign-object

foreign import foldMImpl :: forall a b m z. (m -> (z -> m) -> m) -> (z -> a -> b -> m) -> m -> Map a b -> m

-- | Fold the keys and values of a map
fold :: forall a b z. (z -> a -> b -> z) -> z -> Map a b -> z
fold = foldMImpl ((#))

-- | Fold the keys and values of a map, accumulating values using some
-- | `Monoid`.
foldMap :: forall a b m. Monoid m => (a -> b -> m) -> Map a b -> m
foldMap f = fold (\acc k v -> acc <> f k v) mempty

-- | Fold the keys and values of a map, accumulating values and effects in
-- | some `Monad`.
foldM :: forall a b m z. Monad m => (z -> a -> b -> m z) -> z -> Map a b -> m z
foldM f z = foldMImpl bind f (pure z)

instance foldableMap :: Foldable (Map a) where
  foldl f = fold (\z _ -> f z)
  foldr f z m = foldr f z (values m)
  foldMap f = foldMap (const f)

instance traversableMap :: Traversable (Map a) where
  traverse f ms = fold (\acc k v -> flip (insert k) <$> acc <*> f v) (pure empty) ms
  sequence = sequenceDefault