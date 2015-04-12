
module Test.Hspec.Cascading (CascadingSpec, describe, it, shouldBe, spec) where

import BasePrelude hiding (cast, map)
import Data.DList (DList, map, singleton, snoc)
import qualified Test.Hspec as H


type CascadingSpec = CascadingSpecM ()

data CascadingSpecM a =
	CExpectation (IO ()) |
	CSpec String (DList (CascadingSpecM a))


instance Monad CascadingSpecM where
	(CSpec title specs) >> spec = CSpec title (snoc (map cast specs) spec)
	(>>=) = error "Not Implemented"
	return = error "Not Implemented"

cast :: CascadingSpecM a -> CascadingSpecM b
cast (CExpectation io) = CExpectation io
cast (CSpec title specs) = CSpec title (map cast specs)


spec :: CascadingSpec -> H.Spec
spec (CSpec title specs) = undefined

describe, it :: String -> CascadingSpec -> CascadingSpec
it title spec = CSpec title (singleton spec)
describe = it

shouldBe :: (Eq a, Show a) => a -> a -> CascadingSpec
shouldBe x y = CExpectation (H.shouldBe x y)
