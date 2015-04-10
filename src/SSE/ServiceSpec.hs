
module SSE.ServiceSpec where

import SSE.Service

import Control.Concurrent.MVar
import Data.Map.Strict as M
import Test.Hspec


spec :: Spec
spec = describe "SSErvice" $ do
	it "should be empty when created" $ do
		service <- sservice (const "key")
		state <- readMVar (getState service)
		size (getChannels state) `shouldBe` 0
		size (getKeys state) `shouldBe` 0
