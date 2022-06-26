module HaskellHutton.Chapter5 where

import Data.Char

-- Conditionals
x' :: [Integer]
x' = [x ^ 2 | x <- [1 .. 5]]

x'' :: [(Integer, Char)]
x'' = [(x, y) | x <- [1 .. 5], y <- ['a' .. 'z']]

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [a | (a, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime x = length (factors x) == 2

primes :: [Int]
primes = [x | x <- [1 ..], isPrime x]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [a <= b | (a, b) <- pairs xs]

lowers :: [Char] -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Eq a => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x == x']

-- Caesar cipher
positions :: (Num a1, Enum a1, Eq a2) => a2 -> [a2] -> [a1]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

letterToInt :: Char -> Int
letterToInt c = ord c - ord 'a'

intToLetter :: Int -> Char
intToLetter n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = intToLetter ((letterToInt c + n) `mod` 26)
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

percent :: (Fractional a1, Integral a2, Integral a3) => a2 -> a3 -> a1
percent n m = (fromIntegral n / fromIntegral m) / 100

freqs :: Fractional a => [Char] -> [a]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lowers xs

chisqr :: Fractional a => [a] -> [a] -> a
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

crack :: String -> String
crack xs = encode (- factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

-- Exercises
squares :: Integer
squares = sum [x ^ 2 | x <- [1 .. 100]]

grid :: (Num a, Num b, Enum a, Enum b) => a -> b -> [(a, b)]
grid n m = [(x, y) | x <- [0 .. n], y <- [0 .. m]]

gridSquares :: (Num b, Eq b, Enum b) => b -> [(b, b)]
gridSquares n = [(x ^ 2, y ^ 2) | (x, y) <- grid n n, x /= y]

replicate' :: (Num t, Enum t) => t -> a -> [a]
replicate' n x = [x | _ <- [1 .. n]]

pyths :: (Num c, Enum c, Eq c) => c -> [(c, c, c)]
pyths n =
    [ (x, y, z)
    | x <- [1 .. n]
    , y <- [1 .. n]
    , z <- [1 .. n]
    , x * x + y * y == z * z
    ]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (init $ factors x) == x]
