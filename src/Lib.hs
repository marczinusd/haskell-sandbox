module Lib (someFunc, greet) where

import           HaskellBook.Test (hello)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

greet :: String
greet = "hello!"

anotherGreet :: [Char]
anotherGreet = hello
