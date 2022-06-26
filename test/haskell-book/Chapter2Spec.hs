module Chapter2Spec where

import HaskellBook.Chapter2 (someStuff)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "someStuff" $
        do
            it "returns the first element of a list" $
                do
                    someStuff `shouldBe` "hello"
