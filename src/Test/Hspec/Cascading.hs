
module Test.Hspec.Cascading (CascadingSpec, describe, it, shouldBe) where

import BasePrelude hiding (cast)
import Data.DList (DList, append, cons, singleton, snoc)
import qualified Data.DList as D (map)
import Test.Hspec (Arg, Expectation, Spec, SpecWith)
import qualified Test.Hspec as H (describe, it, shouldBe)


type CascadingSpec = CascadingSpecM ()

data CascadingSpecM a = CSpec {
	getTitle :: String,
	getExpect :: IO (),
	getSpecs :: DList (CascadingSpecM a)
} | CExpectation (IO ())


instance Monad CascadingSpecM where
	(>>) (CExpectation _) = error "Hell!"
	(>>) (CSpec title expect specs) = \case
		CSpec title' expect' specs' -> CSpec title expect (snoc (D.map cast specs) (CSpec title' expect' specs'))
		CExpectation expect' -> CSpec title (expect >> expect') (D.map cast specs)
	(>>=) = error "Not Implemented"
	return = error "Not Implemented"

cast :: CascadingSpecM a -> CascadingSpecM b
cast (CExpectation io) = CExpectation io
cast (CSpec title expect specs) = CSpec title expect (D.map cast specs)


describe :: String -> CascadingSpec -> Spec
describe subject spec = H.describe subject specs where
	specs = sequence_ $ toList $ its spec

its :: CascadingSpec -> DList (SpecWith (Arg Expectation))
its (CSpec title expect specs) = cons (H.it title expect) its' where
	its' = foldr append empty $ D.map its specs


it :: String -> CascadingSpec -> CascadingSpec
it title spec = CSpec title (return ()) empty >> spec

shouldBe :: (Eq a, Show a) => a -> a -> CascadingSpec
shouldBe x y = CExpectation (H.shouldBe x y)
