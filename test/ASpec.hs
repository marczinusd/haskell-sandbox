module ASpec where

import           Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "Prelude.head"
    $ do
      it "returns the first element of a list"
        $ do
          head [23 ..] `shouldBe` (23 :: Int)