
module Data.GroupMapSpec where

import BasePrelude
import Test.Hspec
import qualified Test.Hspec.Cascading as C

import Data.GroupMap as M


{-
spec :: Spec
-- Use cascading tests
spec = describe "GroupMap" $ do
	it "should insert group and value for key" $ do
		let gmap = M.insert 'a' 1 True M.empty
		M.lookup 'a' gmap `shouldBe` Just (1, True)
		M.lookupGroup 1 gmap `shouldBe` Just [('a', True)]
	it "should add key to existing group" $ do
		let gmap = M.insert 'a' 1 True $
			M.insert 'b' 1 False M.empty
		M.lookupGroup 1 gmap `shouldBe` Just [('a', True), ('b', False)]
-}

spec :: Spec
spec = C.describe "GroupMap" $
	C.it "should insert group and value for key" $ do
		let gmap = M.insert 'a' 1 True M.empty
		M.lookup 'a' gmap `C.shouldBe` Just (1, True)
		M.lookupGroup 1 gmap `C.shouldBe` Just [('a', True)]
		C.it "should add key to existing group" $ do
			let gmap = M.insert 'b' 1 False gmap
			M.lookupGroup 1 gmap `C.shouldBe` Just [('a', True), ('b', False)]