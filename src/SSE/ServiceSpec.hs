
module SSE.ServiceSpec where

import SSE.Service

import Control.Concurrent.MVar
import Data.Map.Strict as M
import Test.Hspec


spec :: Spec
spec = describe "SSErvice" $ do
	it "should be empty when created" $ do
		0 `shouldBe` 0
