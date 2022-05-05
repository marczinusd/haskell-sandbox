module HaskellBook.Chapter9 where

import Data.Char (isSpace, isUpper, toUpper)

myWords :: String -> [String]
myWords "" = []
myWords (' ' : xs) = myWords $ dropWhile isSpace xs
myWords sentence =
  [takeWhile notSpace sentence]
    ++ myWords (dropWhile notSpace sentence)
  where
    notSpace = not . isSpace

map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x : xs) = f x : (map2 f xs)

zip2 :: [a] -> [b] -> [(a, b)]
zip2 _ [] = []
zip2 [] _ = []
zip2 (x : xs) (y : ys) = (x, y) : zip2 xs ys

zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 _ _ [] = []
zipWith2 _ [] _ = []
zipWith2 f (x : xs) (y : ys) = f x y : zipWith2 f xs ys

isAllUpper :: String -> Bool
isAllUpper [] = True
isAllUpper (x : xs) = isUpper x && isAllUpper xs

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]
