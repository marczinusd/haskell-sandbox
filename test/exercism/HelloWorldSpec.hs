module HelloWorldSpec where

import           Test.Hspec (Spec, it, shouldBe)
import           Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import           Exercism.HelloWorld (hello)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = it "hello" $ hello `shouldBe` "Hello, World!"
-- 5c542864b011fb742aa95ca950d3473ce168ec8c
