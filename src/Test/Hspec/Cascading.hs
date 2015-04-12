
module Test.Hspec.Cascading where

import BasePrelude hiding (cast)
import Data.DList as D (DList, map, snoc)
import Test.Hspec as H


type CascadingSpec = CascadingSpecM ()

data CascadingSpecM a =
	CExpectation (IO ()) |
	CSpec String (DList (CascadingSpecM a))

instance Monad CascadingSpecM where
	(CSpec title specs) >> spec = CSpec title (snoc (D.map cast specs) spec)
	(>>=) = error "Not Implemented"
	return = error "Not Implemented"

cast :: CascadingSpecM a -> CascadingSpecM b
cast (CExpectation io) = CExpectation io
cast (CSpec title specs) = CSpec title (D.map cast specs)


it :: String -> CascadingSpec -> CascadingSpec
it title spec = undefined

shouldBe :: (Eq a, Show a) => a -> a -> CascadingSpec
shouldBe x y = undefined
