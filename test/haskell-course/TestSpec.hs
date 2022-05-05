module TestSpec where

import           HaskellCourse.Test (hello)
import           Test.Hspec (describe, it, shouldBe, Spec)
import           Test.QuickCheck ()

spec = do
  describe "hello"
    $ do
      it "returns 'hello world'"
        $ do
          hello `shouldBe` "hello world"
