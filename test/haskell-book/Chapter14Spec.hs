module Chapter14Spec where

import           Test.Hspec (describe, it, shouldBe, Spec)
import           HaskellBook.Chapter14 (charToMorse, dividedBy, letterToMorse
                                      , morseToChar, myAdd, Morse)
import           Test.QuickCheck.Property (Testable(property))
import qualified Data.Map as M
import           Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

spec :: Spec
spec = do
  describe "myAdd"
    $ do
      it "1 + 1 is greater than 1"
        $ do
          myAdd 1 1 > 1 `shouldBe` True
      it "x + 1 is always greater than x"
        $ do
          property $ \x -> x + 1 > (x :: Int)
  describe "dividedBy"
    $ do
      it "15 divided by 3 is 5"
        $ do
          15 `dividedBy` 3 `shouldBe` (5, 0)
      it "22 divided by 5 is 4 remainder 2"
        $ do
          22 `dividedBy` 5 `shouldBe` (4, 2)
  describe "morse"
    $ do
      it "should be inverse to toMorse"
        $ do
          forAll
            charGen
            (\c -> (charToMorse c >>= morseToChar) `shouldBe` Just c)
      it "should be inverse to fromMorse"
        $ do
          forAll
            morseGen
            (\c -> (morseToChar c >>= charToMorse) `shouldBe` Just c)
