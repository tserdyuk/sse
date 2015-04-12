
module Data.GroupMap where

import BasePrelude
import Data.Map.Strict as M
	(Map, delete, empty, insert, insertWith, lookup, update, updateLookupWithKey)
import Data.Set as S (Set, delete, singleton, toList, union)


data GroupMap k g v = GM {
	keys :: Map k (g, v),
	groups :: Map g (Set (KeyValue k v))
} deriving (Eq, Show)

newtype KeyValue k v = KV { kv :: (k, v) } deriving (Show)

instance (Eq k) => Eq (KeyValue k v) where
	(KV (k, _)) == (KV (k', _)) = k == k'

instance (Ord k) => Ord (KeyValue k v) where
	compare (KV (k, _)) (KV (k', _)) = compare k k'


empty :: GroupMap k g v
empty = GM M.empty M.empty

lookup :: (Ord k) => k -> GroupMap k g v -> Maybe (g, v)
lookup k (GM ks _) = M.lookup k ks

lookupGroup :: (Ord g) => g -> GroupMap k g v -> Maybe [(k, v)]
lookupGroup g (GM _ gs) = fmap (map kv . S.toList) $ M.lookup g gs

insert :: (Ord k, Ord g) => k -> g -> v -> GroupMap k g v -> GroupMap k g v
insert k g v (GM ks gs) = GM (M.insert k (g, v) ks) gs' where
	gs' = M.insertWith S.union g (S.singleton $ KV (k, v)) gs

delete :: (Ord k, Ord g) => k -> GroupMap k g v -> GroupMap k g v
delete k (GM ks gs) = GM ks' gs' where
	(g, ks') = updateLookupWithKey (const . const Nothing) k ks
	gs' = maybe gs (\(g, v) -> M.update (del v) g gs) g
	del v = Just . S.delete (KV (k, v))
