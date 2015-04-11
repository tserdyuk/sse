
module Data.GroupMap where

import BasePrelude
import Data.Map.Strict as M (Map, empty, insert, insertWith, lookup)
import Data.Set as S (Set, singleton, toList, union)


data GroupMap k g v = GM {
	keys :: Map k (g, v),
	groups :: Map g (Set (k, v))
} deriving (Eq, Show)


empty :: GroupMap k g v
empty = GM M.empty M.empty

lookup :: (Ord k) => k -> GroupMap k g v -> Maybe (g, v)
lookup k (GM ks _) = M.lookup k ks

lookupGroup :: (Ord g) => g -> GroupMap k g v -> Maybe [(k, v)]
lookupGroup g (GM _ gs) = fmap S.toList $ M.lookup g gs

insert :: (Ord k, Ord g, Ord v) => k -> g -> v -> GroupMap k g v -> GroupMap k g v
insert k g v (GM ks gs) = GM (M.insert k (g, v) ks) gs' where
	gs' = M.insertWith S.union g (S.singleton (k, v)) gs

delete :: (Ord k) => k -> GroupMap k g v -> GroupMap k g v
delete = undefined
