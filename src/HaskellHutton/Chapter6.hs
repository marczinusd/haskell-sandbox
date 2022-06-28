module HaskellHutton.Chapter6 where

import Control.Exception.Base (runtimeError)

-- Exercises

fact :: Integral a => a -> a
fact n
    | n <= 0 = 1
    | otherwise = n * fact (n - 1)

sumDown :: (Ord a, Num a) => a -> a
sumDown n
    | n <= 0 = 0
    | otherwise = n + sumDown (n - 1)

pow :: (Num a1, Ord a1, Fractional a2) => a2 -> a1 -> a2
pow x n
    | n == 0 = 1
    | n <= 0 = 1 / pow x (- n)
    | otherwise = x * pow x (n - 1)

euclid :: (Ord a, Num a) => a -> a -> a
euclid x y
    | x == y = x
    | x > y = euclid (x - y) y
    | otherwise = euclid x (y - x)

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = append xs (concat' xss)
  where
    append [] [] = []
    append xs [] = xs
    append [] ys = ys
    append (x : xs) ys = x : append xs ys

replicate'' :: (Ord t, Num t) => a -> t -> [a]
replicate'' x n
    | n <= 0 = []
    | otherwise = x : (replicate'' x (n - 1))

(!!!) :: (Eq t, Num t) => [p] -> t -> p
(!!!) (x : xs) n
    | n == 0 = x
    | otherwise = xs !!! (n - 1)
(!!!) [] _ = error "Index too large"

elem' :: Eq t => t -> [t] -> Bool
elem' x (y : ys) = x == y || elem' x ys
elem' _ [] = False

merge' :: Ord a => [a] -> [a] -> [a]
merge' xs [] = xs
merge' [] ys = ys
merge' (x : xs) (y : ys)
    | x > y = y : merge' (x : xs) ys
    | otherwise = x : merge' xs (y : ys)
