
module Test.Hspec.Cascading (CascadingSpec, describe, it, shouldBe) where

import BasePrelude hiding (cast)
import Data.DList (DList, append, cons, fromList, singleton, snoc)
import qualified Data.DList as D (map)
import Test.Hspec (Arg, Expectation, Spec, SpecWith)
import qualified Test.Hspec as H (describe, it, shouldBe)


type CascadingSpec = CSpecExpr ()

data CSpecExpr a = CSpecExpr {
	getTitle :: Maybe String,
	getExpect :: IO (),
	getSpecs :: DList (CSpecExpr a)
}


instance Monad CSpecExpr where
	-- Refactor with pattern synonyms
	(CSpecExpr Nothing expect specs) >> (CSpecExpr Nothing expect' specs') =
		CSpecExpr Nothing (expect >> expect') (append (D.map cast specs) specs')
	(CSpecExpr Nothing expect specs) >> spec@(CSpecExpr (Just _) _ _) =
		CSpecExpr Nothing expect (snoc (D.map cast specs) spec)
	spec@(CSpecExpr (Just _) _ _) >> (CSpecExpr Nothing expect' specs') =
		CSpecExpr Nothing expect' (cons (cast spec) specs')
	spec@(CSpecExpr (Just _) _ _) >> spec'@(CSpecExpr (Just _) _ _) =
		CSpecExpr Nothing (return ()) (fromList [cast spec, spec'])

	(>>=) = error "Not Implemented"
	return = error "Not Implemented"

cast :: CSpecExpr a -> CSpecExpr b
cast (CSpecExpr title expect specs) = CSpecExpr title expect (D.map cast specs)


describe :: String -> CascadingSpec -> Spec
describe subject spec = H.describe subject specs where
	specs = sequence_ $ toList $ its spec

its :: CascadingSpec -> DList (SpecWith (Arg Expectation))
its (CSpecExpr (Just title) expect specs) = cons (H.it title expect) its' where
	its' = foldr append empty $ D.map its specs


it :: String -> CascadingSpec -> CascadingSpec
it title (CSpecExpr Nothing expect specs) = CSpecExpr (Just title) expect specs

shouldBe :: (Eq a, Show a) => a -> a -> CascadingSpec
shouldBe x y = CSpecExpr Nothing (H.shouldBe x y) empty
