module Erl.Data.Map
  ( Map
  , empty
  , isEmpty
  , insert
  , singleton
  , lookup
  , values
  , mapWithKey
  , member
  , delete
  , fromFoldable
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..))
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

foreign import memberImpl :: forall k a. k -> Map k a -> Boolean

member :: forall k a. k -> Map k a -> Boolean
member = memberImpl

foreign import deleteImpl :: forall k a. k -> Map k a -> Map k a

delete :: forall k a. k -> Map k a -> Map k a
delete = deleteImpl

foreign import values :: forall a b. Map a b -> List b

-- Folds taken from purescript-foreign-object

foreign import foldMImpl :: forall a b m z. (m -> (z -> m) -> m) -> (z -> a -> b -> m) -> m -> Map a b -> m

-- | Fold the keys and values of a map
fold :: forall a b z. (z -> a -> b -> z) -> z -> Map a b -> z
fold = foldMImpl ((#))

-- | Fold the keys and values of a map, accumulating values using some
-- | `Monoid`.
foldMap :: forall a b m. Monoid m => (a -> b -> m) -> Map a b -> m
foldMap f = fold (\acc k v -> f k v <> acc) mempty

-- | Fold the keys and values of a map, accumulating values and effects in
-- | some `Monad`.
foldM :: forall a b m z. Monad m => (z -> a -> b -> m z) -> z -> Map a b -> m z
foldM f z = foldMImpl bind f (pure z)

-- | Convert any foldable collection of key/value pairs to a map.
-- | On key collision, later values take precedence over earlier ones.
fromFoldable :: forall f k v. Ord k => Foldable f => f (Tuple k v) -> Map k v
fromFoldable = foldl (\m (Tuple k v) -> insert k v m) empty

--foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
--foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
--foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
instance foldableMap :: Foldable (Map a) where
  foldr f z m = foldr f z (values m)
  foldl f = fold (\z _ -> f z)
  foldMap f = foldMap (const f)


--foldrWithIndex :: forall a b. (i -> a -> b -> b) -> b -> f a -> b
--foldlWithIndex :: forall a b. (i -> b -> a -> b) -> b -> f a -> b
--foldMapWithIndex :: forall a m. Monoid m => (i -> a -> m) -> f a -> m

instance foldableWithIndexMap :: FoldableWithIndex a (Map a) where
  foldrWithIndex f = fold (\b i a -> f i a b)
  foldlWithIndex f = fold (\b i a -> f i b a)
  foldMapWithIndex = foldMap

instance traversableMap :: Traversable (Map a) where
  traverse f ms = fold (\acc k v -> flip (insert k) <$> acc <*> f v) (pure empty) ms
  sequence = sequenceDefault
