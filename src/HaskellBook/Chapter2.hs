module HaskellBook.Chapter2 (someStuff) where

someStuff :: String
someStuff = "hello"

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple :: Num a => a -> a
triple x = x * 3

printInc n = print $ plusTwo + plusThree
  where
    plusTwo = n + 2

    plusThree = n + 3

printInc2 n = let plusTwo = n + 2
                  plusThree = n + 3
                  plusFour x = n + x
              in print $ plusTwo + plusThree + plusFour 3
