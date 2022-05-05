module HaskellBook.Chapter10 where

xs = map show [1 .. 5]

y = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs

myFoldl :: (a -> b -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f x acc) xs

stops = "pbtdkg"

vowels = "aeiou"

x = [(x, y) | x <- stops, x == 'p', y <- vowels]
