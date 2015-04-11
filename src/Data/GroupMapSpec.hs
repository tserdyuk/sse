
module Data.GroupMapSpec where

import BasePrelude
import Test.Hspec

import Data.GroupMap as M


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