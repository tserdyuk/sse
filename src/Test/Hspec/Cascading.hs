
module Test.Hspec.Cascading where

import BasePrelude
import Test.Hspec as H


data CascadingSpec =
	CExpectation (IO ()) |
	CSpec String CascadingSpec


it :: String -> CascadingSpec -> CascadingSpec
it (CSpec name specs) = undefined

shouldBe :: (Eq a, Show a) => a -> a -> CascadingSpec
shouldBe x y = undefined
