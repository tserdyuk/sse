
module Data.GroupMap where

import BasePrelude
import Data.Map.Strict (Map)
import Data.Set (Set)


data GroupMap k g v = GM {
	keys :: Map k (g, v),
	groups :: Map g (Set k)
}


empty :: GroupMap k g v
empty = undefined

lookup :: (Ord k) => k -> GroupMap k g v -> Maybe (g, v)
lookup = undefined

lookupGroup :: (Ord g) => g -> GroupMap k g v -> Maybe [(k, v)]
lookupGroup = undefined

insert :: (Ord k, Ord g) => k -> g -> v -> GroupMap k g v -> GroupMap k g v
insert = undefined

delete :: (Ord k) => k -> GroupMap k g v -> GroupMap k g v
delete = undefined
