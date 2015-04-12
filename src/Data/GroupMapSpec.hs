
module Data.GroupMapSpec where

import BasePrelude
import Test.Hspec.Cascading

import Data.GroupMap as M

spec :: Spec
spec = describe "GroupMap" $
	it "should insert group and value for key" $ do
		let gmap = M.insert 'a' 1 True M.empty
		M.lookup 'a' gmap `shouldBe` Just (1, True)
		M.lookupGroup 1 gmap `shouldBe` Just [('a', True)]
		it "should add key to existing group" $ do
			let gmap2 = M.insert 'b' 1 False gmap
			M.lookupGroup 1 gmap2 `shouldBe` Just [('a', True), ('b', False)]