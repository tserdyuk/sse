
module Data.GroupMapSpec where

import Test.Hspec

import Data.GroupMap as M


spec :: Spec
-- Use cascading tests
spec = describe "GroupMap" $ do
	it "should insert group and value for key" $ do
		let gmap = insert 'a' 1 True empty
		M.lookup 'a' gmap `shouldBe` Just (1, True)
		M.lookupGroup 1 gmap `shouldBe` Just [('a', True)]
	it "should add key to existing group" $ do
		let gmap = insert 'a' 1 True $
			insert 'b' 1 False empty
		M.lookupGroup 1 gmap `shouldBe` Just [('a', True), ('b', False)]