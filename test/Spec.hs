import           Control.Exception (evaluate)
import           Lib (greet)
import           Test.Hspec
import           Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (property)

main :: IO ()
main = hspec
  $ do
    describe "Greet"
      $ do
        it "returns hello!"
          $ do
            greet `shouldBe` "hello!"
    describe "read"
      $ do
        modifyMaxSuccess (const 1000)
          $ it "is inverse to show"
          $ property
          $ \x -> (read . show) x == (x :: Int)
