import           Control.Exception (evaluate)
import           Lib (greet)
import           Test.Hspec

main :: IO ()
main = hspec
  $ do
    describe "Greet"
      $ do
        it "returns hello!"
          $ do
            greet `shouldBe` "hello!"
