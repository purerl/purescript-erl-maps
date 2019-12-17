module Erl.Data.Map.Internal
  ( Map
  , empty
  , isEmpty
  , size
  , insert
  , singleton
  , lookup
  , values
  , keys
  , mapWithKey
  , member
  , delete
  , difference
  , fromFoldable
  , alter
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..))
import Erl.Data.List (List)

foreign import data Map :: Type -> Type -> Type

-- | An empty map
foreign import empty :: forall a b. Map a b

-- | Test if a map is empty
foreign import isEmpty :: forall a b. Map a b -> Boolean

-- | Calculate the number of key/value pairs in a map
foreign import size :: forall a b. Map a b -> Int

-- | Insert or replace a key/value pair in a map
foreign import insert :: forall a b. a -> b -> Map a b -> Map a b

-- | Test if a key is a member of a map
foreign import member :: forall k a. k -> Map k a -> Boolean

-- | Difference of two maps. Return elements of the first map where
-- | the keys do not exist in the second map.
foreign import difference :: forall k a b. Map k a -> Map k b -> Map k a

-- | Delete a key and its corresponding value from a map.
foreign import delete :: forall k a. k -> Map k a -> Map k a

-- | Get a list of the values contained in a map
foreign import values :: forall a b. Map a b -> List b

-- | Get a list of the keys contained in a map
foreign import keys :: forall a b. Map a b -> List a

-- Folds taken from purescript-foreign-object
foreign import foldMImpl
  :: forall a b m z.
  (m -> (z -> m) -> m)
  -> (z -> a -> b -> m)
  -> m
  -> Map a b
  -> m

foreign import lookupImpl :: forall a b z. z -> (b -> z) -> a -> Map a b -> z
foreign import mapImpl :: forall k a b. (a -> b) -> Map k a -> Map k b
foreign import mapWithKeyImpl :: forall k a b. (Fn2 k a b) -> Map k a -> Map k b
-- foreign import unfoldrMapImpl :: forall a b t. Unfoldable t => (b -> Maybe (Tuple a b)) -> b -> t a


instance functorMap :: Functor (Map a) where
  map f m = mapImpl f m

instance foldableMap :: Foldable (Map a) where
  foldr f z m = foldr f z (values m)
  foldl f = fold (\z _ -> f z)
  foldMap f = foldMap (const f)

-- instance unfoldableMap :: Unfoldable (Map a) where
--   unfoldr f m = ?v

instance foldableWithIndexMap :: FoldableWithIndex a (Map a) where
  foldrWithIndex f = fold (\b i a -> f i a b)
  foldlWithIndex f = fold (\b i a -> f i b a)
  foldMapWithIndex = foldMap

instance traversableMap :: Traversable (Map a) where
  traverse f ms = fold (\acc k v -> flip (insert k) <$> acc <*> f v) (pure empty) ms
  sequence = sequenceDefault


-- | Create a map with one key/value pair
singleton :: forall a b. a -> b -> Map a b
singleton a b = insert a b empty

-- | Look up a value for the specified key
lookup :: forall a b. a -> Map a b -> Maybe b
lookup = lookupImpl Nothing Just

mapWithKey :: forall k a b. (k -> a -> b) -> Map k a -> Map k b
mapWithKey f m = mapWithKeyImpl (mkFn2 f) m

-- | Insert the value, delete a value, or update a value for a key in a map
alter :: forall k v. (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k m = lookup k m # f # maybe' (\_ -> delete k m) (\v -> insert k v m)

-- | Update or delete the value for a key in a map
update :: forall k v. Ord k => (v -> Maybe v) -> k -> Map k v -> Map k v
update f k m = alter (maybe Nothing f) k m

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

-- | Convert any foldable collection of key/value pairs to a map.
-- | On key collision, the values are configurably combined.
fromFoldableWith :: forall f k v. Ord k => Foldable f => (v -> v -> v) -> f (Tuple k v) -> Map k v
fromFoldableWith f = foldl (\m (Tuple k v) -> alter (combine v) k m) empty where
  combine v (Just v') = Just $ f v v'
  combine v Nothing = Just v

-- | Convert any indexed foldable collection into a map.
fromFoldableWithIndex :: forall f k v. Ord k => FoldableWithIndex k f => f v -> Map k v
fromFoldableWithIndex = foldlWithIndex (\k m v -> insert k v m) empty

-- | Convert a map to an unfoldable structure of key/value pairs where the keys are in ascending order
-- toUnfoldable :: forall f k v. Unfoldable f => Map k v -> f (Tuple k v)
-- toUnfoldable m = ?a
