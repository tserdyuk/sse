
module Test.Hspec.Cascading (CascadingSpec, describe, it, shouldBe) where

import BasePrelude hiding (cast)
import Data.DList (DList, append, singleton, snoc)
import qualified Data.DList as D (map)
import Test.Hspec (Arg, Expectation, Spec, SpecWith)
import qualified Test.Hspec as H (describe, it, shouldBe)


type CascadingSpec = CascadingSpecM ()

data CascadingSpecM a =
	CExpectation { expect :: IO () } |
	CSpec String (DList (CascadingSpecM a))

isExpect :: CascadingSpec -> Bool
isExpect = \case
	CExpectation _ -> True
	_ -> False


instance Monad CascadingSpecM where
	(CSpec title specs) >> spec = CSpec title (snoc (D.map cast specs) spec)
	(>>=) = error "Not Implemented"
	return = error "Not Implemented"

cast :: CascadingSpecM a -> CascadingSpecM b
cast (CExpectation io) = CExpectation io
cast (CSpec title specs) = CSpec title (D.map cast specs)


describe :: String -> CascadingSpec -> Spec
describe subject (CSpec title specs) = H.describe subject undefined

spec :: DList CascadingSpec -> CascadingSpec -> SpecWith (Arg Expectation)
spec xs (CSpec title ys) = H.it title (sequence_ exps) where
	exps = map expect $ filter isExpect $ toList $ append xs ys


it :: String -> CascadingSpec -> CascadingSpec
it title spec = CSpec title (singleton spec)

shouldBe :: (Eq a, Show a) => a -> a -> CascadingSpec
shouldBe x y = CExpectation (H.shouldBe x y)
