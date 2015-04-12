
module Test.Hspec.Cascading (CascadingSpec, describe, it, shouldBe) where

import BasePrelude hiding (cast, map)
import Data.DList (DList, map, singleton, snoc)
import qualified Test.Hspec as H (Spec, it, shouldBe)


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


describe :: String -> CascadingSpec -> H.Spec
describe subject (CSpec title specs) = undefined

it :: String -> CascadingSpec -> CascadingSpec
it title spec = CSpec title (singleton spec)

shouldBe :: (Eq a, Show a) => a -> a -> CascadingSpec
shouldBe x y = CExpectation (H.shouldBe x y)
