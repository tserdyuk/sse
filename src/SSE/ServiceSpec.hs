
module SSE.ServiceSpec where

import BasePrelude
import Test.Hspec

import SSE.Service


spec :: Spec
spec = describe "SSErvice" $ do
	it "should be empty when created" $ do
		0 `shouldBe` 0
