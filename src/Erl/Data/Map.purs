module Erl.Data.Map
  ( Map
  , alter
  , alterM
  , catMaybes
  , delete
  , difference
  , empty
  , filter
  , filterKeys
  , filterWithKey
  , foldM
  , foldr
  , foldl
  , fromFoldable
  , fromFoldableWith
  , fromFoldableWithIndex
  , insert
  , insertWith
  , intersection
  , intersectionWith
  , isEmpty
  , keys
  , lookup
  , mapMaybe
  , mapMaybeWithKey
  , mapWithKey
  , member
  , singleton
  , size
  , separate 
  , toUnfoldable
  , toUnfoldableUnordered
  , union
  , unionWith
  , unions
  , update
  , updateM
  , values
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Compactable (class Compactable)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Filterable (class Filterable)
import Data.Foldable (class Foldable)
import Data.Foldable (foldl) as Foldable
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, mkFn3)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndexDefault)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Erl.Data.List (List)
import Erl.Data.List as List

foreign import data Map :: Type -> Type -> Type

foreign import empty :: forall a b. Map a b

foreign import isEmpty :: forall a b. Map a b -> Boolean

foreign import size :: forall a b. Map a b -> Int

foreign import insert :: forall a b. a -> b -> Map a b -> Map a b

-- | Inserts or updates a value with the given function.
-- |
-- | The combining function is called with the existing value as the first
-- | argument and the new value as the second argument.
insertWith :: forall k v. (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v = alter (Just <<< maybe v (flip f v)) k

foreign import filterWithKeyImpl :: forall k v. (Fn2 k v Boolean) -> Map k v -> Map k v

filterWithKey :: forall k v. (k -> v -> Boolean) -> Map k v -> Map k v
filterWithKey pred = filterWithKeyImpl (mkFn2 pred)

singleton :: forall a b. a -> b -> Map a b
singleton a b = insert a b empty

foreign import lookupImpl :: forall a b z. z -> (b -> z) -> a -> Map a b -> z

-- Get the value that corresponds to the key, if it exists.
lookup :: forall a b. a -> Map a b -> Maybe b
lookup = lookupImpl Nothing Just

foreign import mapImpl :: forall k a b. (a -> b) -> Map k a -> Map k b

instance functorMap :: Functor (Map a) where
  map f m = mapImpl f m

foreign import mapWithKeyImpl :: forall k a b. (Fn2 k a b) -> Map k a -> Map k b

mapWithKey :: forall k a b. (k -> a -> b) -> Map k a -> Map k b
mapWithKey f = mapWithKeyImpl (mkFn2 f)

-- | Applies a function to each value in a map, discarding entries where the
-- | function returns `Nothing`.
mapMaybe :: forall k a b. (a -> Maybe b) -> Map k a -> Map k b
mapMaybe = mapMaybeWithKey <<< const

-- | Applies a function to each key/value pair in a map, discarding entries
-- | where the function returns Nothing.
mapMaybeWithKey :: forall k a b. (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey f = foldl (\acc k a -> maybe acc (\b -> insert k b acc) (f k a)) empty

-- | Filter a map of optional values, keeping only the key/value pairs which
-- | contain a value, creating a new map.
catMaybes :: forall k v. Map k (Maybe v) -> Map k v
catMaybes = mapMaybe identity

foreign import member :: forall k a. k -> Map k a -> Boolean

foreign import difference :: forall k a b. Map k a -> Map k b -> Map k a

foreign import intersectionWithImpl :: forall k a b c. (Fn2 a b c) -> Map k a -> Map k b -> Map k c

intersectionWith :: forall k a b c. (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f m1 m2 =
  intersectionWithImpl (mkFn2 f) m1 m2

foreign import delete :: forall k a. k -> Map k a -> Map k a

foreign import values :: forall a b. Map a b -> List b

foreign import keys :: forall a b. Map a b -> List a

-- | Compute the union of two maps.
-- | Note: Now this function keeps the values from the first argument, to match Data.Map. Previously this function kept the values from the second argument.
foreign import union :: forall k v. Map k v -> Map k v -> Map k v

foreign import unionWithImpl :: forall k v. (Fn2 v v v) -> Map k v -> Map k v -> Map k v

-- | Compute the union of two maps, using the specified function
-- | to combine values for duplicate keys.
-- | Note: Now this function keeps the values from the first argument, to match Data.Map. Previously this function kept the values from the second argument.
unionWith :: forall k v. (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f m1 m2 =
  unionWithImpl (mkFn2 f) m1 m2

-- | Compute the union of a collection of maps. Keeps the first value for conflicting keys.
-- | Note: Now this function keeps the values from the first argument, to match Data.Map. Previously this function kept the values from the second argument.
unions :: forall k v f. Foldable f => f (Map k v) -> Map k v
unions = Foldable.foldl union empty

-- | Insert the value, delete a value, or update a value for a key in a map
alter :: forall k v. (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k m = case lookup k m of
  Nothing -> maybe m (\v -> insert k v m) $ f Nothing
  org -> maybe' (\_ -> delete k m) (\v -> insert k v m) $ f org

alterM :: forall k v m. Functor m => (Maybe v -> m (Maybe v)) -> k -> Map k v -> m (Map k v)
alterM f k m = case lookup k m of
  Nothing -> maybe m (\v -> insert k v m) <$> f Nothing
  org -> maybe' (\_ -> delete k m) (\v -> insert k v m) <$> f org

-- | Update or delete the value for a key in a map
update :: forall k v. (v -> Maybe v) -> k -> Map k v -> Map k v
update f k m = alter (maybe Nothing f) k m

updateM :: forall k v m. Applicative m => (v -> m (Maybe v)) -> k -> Map k v -> m (Map k v)
updateM f k m = alterM (maybe (pure Nothing) f) k m

-- foreign import foldImpl :: forall a b z. (Fn3 a b z z) -> z -> Map a b -> z

foreign import foldlImpl :: forall a b z. (Fn3 a b z z) -> z -> Map a b -> z
foreign import foldrImpl :: forall a b z. (Fn3 a b z z) -> z -> Map a b -> z

foldl :: forall a b z. (z -> a -> b -> z) -> z -> Map a b -> z
foldl f = foldlImpl (mkFn3 \a b z -> f z a b)

foldr :: forall a b z. (a -> b -> z -> z) -> z -> Map a b -> z
foldr f = foldrImpl (mkFn3 \a b z -> f a b z)

foreign import foldMImpl :: forall a b m z. (m -> (z -> m) -> m) -> (z -> a -> b -> m) -> m -> Map a b -> m
-- | Fold the keys and values of a map, accumulating values using some
-- | `Monoid`.
foldMap :: forall a b m. Monoid m => (a -> b -> m) -> Map a b -> m
foldMap f = foldl (\acc k v -> f k v <> acc) mempty

-- | Fold the keys and values of a map, accumulating values and effects in
-- | some `Monad`.
foldM :: forall a b m z. Monad m => (z -> a -> b -> m z) -> z -> Map a b -> m z
foldM f z = foldMImpl bind f (pure z)

-- | Convert any foldable collection of key/value pairs to a map.
-- | On key collision, later values take precedence over earlier ones.
fromFoldable :: forall f k v. Foldable f => f (Tuple k v) -> Map k v
fromFoldable = Foldable.foldl (\m (Tuple k v) -> insert k v m) empty

-- | Convert any foldable collection of key/value pairs to a map.
-- | On key collision, the values are configurably combined.
fromFoldableWith :: forall f k v. Foldable f => (v -> v -> v) -> f (Tuple k v) -> Map k v
fromFoldableWith f vs = Foldable.foldl (\m (Tuple k v) -> alter (combine v) k m) empty vs where
  combine v (Just v') = Just $ f v v'
  combine v Nothing = Just v

-- | Convert any indexed foldable collection into a map. E.g. `Array` or `List`.
fromFoldableWithIndex :: forall f k v. FoldableWithIndex k f => f v -> Map k v
fromFoldableWithIndex = foldlWithIndex (\k m v -> insert k v m) empty

foreign import toUnfoldableImpl :: forall k v. (Fn2 k v (Tuple k v)) -> Map k v -> List (Tuple k v)
foreign import toUnfoldableUnorderedImpl :: forall k v. (Fn2 k v (Tuple k v)) -> Map k v -> List (Tuple k v)

-- | Convert a map to an unfoldable structure of key/value pairs where the keys are in ascending order. E.g. `Array` or `List`.
toUnfoldable :: forall f k v. Unfoldable f => Map k v -> f (Tuple k v)
toUnfoldable = List.toUnfoldable <$> toUnfoldableImpl (mkFn2 Tuple)

-- | Convert a map to an unfoldable structure of key/value pairs. E.g. `Array` or `List`.
toUnfoldableUnordered :: forall f k v. Unfoldable f => Map k v -> f (Tuple k v)
toUnfoldableUnordered = List.toUnfoldable <$> toUnfoldableUnorderedImpl (mkFn2 Tuple)

toAscArray :: forall k v. Map k v -> Array (Tuple k v)
toAscArray = toUnfoldable

instance eq1Map :: Eq k => Eq1 (Map k) where
  eq1 = eq

instance eqMap :: (Eq k, Eq v) => Eq (Map k v) where
  eq m1 m2 = toAscArray m1 == toAscArray m2

instance ord1Map :: Ord k => Ord1 (Map k) where
  compare1 = compare

instance ordMap :: (Ord k, Ord v) => Ord (Map k v) where
  compare m1 m2 = compare (toAscArray m1) (toAscArray m2)

instance semigroupMap :: Semigroup v => Semigroup (Map k v) where
  append m1 m2 = unionWith append m1 m2

instance monoidSemigroupMap :: Semigroup v  => Monoid (Map k v) where
  mempty = empty

instance foldableMap :: Foldable (Map a) where
  foldr f z m = foldr (\_ v acc -> f v acc) z m
  foldl f z m = foldl (\acc _ v -> f acc v) z m
  foldMap f = foldMap (\_ v -> f v)

instance foldableWithIndexMap :: FoldableWithIndex a (Map a) where
  foldrWithIndex f = foldr (\i v z -> f i v z)
  foldlWithIndex f = foldl (\i z v -> f z i v)
  foldMapWithIndex = foldMap

instance traversableMap :: Traversable (Map a) where
  traverse f ms = foldl (\acc k v -> flip (insert k) <$> acc <*> f v) (pure empty) ms
  sequence = sequenceDefault

instance functorWithIndexMap :: FunctorWithIndex a (Map a) where
  mapWithIndex = mapWithKey

instance traversableWithIndex :: TraversableWithIndex a (Map a) where
  traverseWithIndex = traverseWithIndexDefault

instance showMap :: (Show k, Show v) => Show (Map k v) where
  show m = "(fromFoldable " <> show (toList m) <> ")"
    where
      toList :: forall k' v'. Map k' v' -> List (Tuple k' v')
      toList = toUnfoldable

instance altMap :: Alt (Map k) where
  alt = union

instance plusMap :: Plus (Map k) where
  empty = empty

instance applyMap :: Apply (Map k) where
  apply = intersectionWith identity

instance bindMap :: Bind (Map k) where
  bind m f = mapMaybeWithKey (\k -> lookup k <<< f) m

-- | Partitions the map into two parts, depending on the containing `Either` constructor.
separate :: forall k a b. Map k (Either a b) -> { left :: Map k a, right :: Map k b }
separate m = foldlWithIndex go { left: empty, right: empty } m
  where
    go i { left, right } (Left e)  = { left: insert i e left, right }
    go i { left, right } (Right e) = { left, right: insert i e right }

instance compactMap :: Compactable (Map k) where
  compact = catMaybes 
  separate = separate 

instance filterableMap :: Filterable (Map k) where
  partitionMap f m = separate (map f m)

  partition f m = foldlWithIndex go { yes: empty, no: empty } m
    where
      go i { yes, no } e | f e = { yes: insert i e yes, no }
      go i { yes, no } e       = { yes, no: insert i e no }

  filterMap f m = catMaybes (map f m)
  filter = filter

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the key fails to hold. `true -> keep`.
filterKeys :: forall k. (k -> Boolean) -> Map k ~> Map k
filterKeys predicate = filterWithKey $ const <<< predicate

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the value fails to hold. `true -> keep`.
filter :: forall k v. (v -> Boolean) -> Map k v -> Map k v
filter predicate = filterWithKey $ const predicate

-- | Compute the intersection of two maps, preferring values from the first map in the case
-- | of duplicate keys.
intersection :: forall k a b. Map k a -> Map k b -> Map k a
intersection = intersectionWith const
