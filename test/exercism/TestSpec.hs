module TestSpec where

import Exercism.Test (hello)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck ()

spec = do
    describe "hello" $
        do
            it "returns 'hello world'" $
                do
                    hello `shouldBe` "hello world"
