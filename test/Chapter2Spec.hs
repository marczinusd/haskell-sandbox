module Chapter2Spec where

import           Chapter2 (someStuff)
import           Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "someStuff"
    $ do
      it "returns the first element of a list"
        $ do
          someStuff `shouldBe` "hello"