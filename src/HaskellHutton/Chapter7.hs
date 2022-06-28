module HaskellHutton.Chapter7 where

import Data.Char (chr, ord)
import Debug.Trace
import HaskellHutton.Chapter6 (pow)

twice :: (b -> b) -> b -> b
twice f = f . f

map' :: (t -> a) -> [t] -> [a]
map' f [] = []
map' f (x : xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x : xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f x = [y | y <- x, f y]

-- using foldr
sum' :: (Foldable t, Num b) => t b -> b
sum' = foldr (+) 0

product' :: (Foldable t, Num b) => t b -> b
product' = foldr (*) 1

-- Composition (.) operator
(>>) :: (b -> c) -> (a -> b) -> (a -> c)
(>>) f g = \x -> f (g x)

-- Base conversion
type Bit = Int

bin2int' :: Num a => [a] -> a
bin2int' bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

bin2int'' :: [Bit] -> Int
bin2int'' [] = 0
bin2int'' (x : xs) = x + 2 * bin2int'' xs

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin x = x `mod` 2 : int2bin (x `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- Exercises
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f f' = map f . filter f'

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x y -> f x : y) []

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' f = foldr (\x y -> if f x then x : y else y) []

dec2int :: (Foldable t, Num a, Show a) => t a -> a
dec2int = foldl (\x y -> trace ("10 * " ++ show x ++ " + " ++ show y) 10 * x + y) 0

dec2int' :: Num p => [p] -> p
dec2int' [] = 0
dec2int' xs = go xs 0
  where
    go [] acc = acc
    go (x : xs) acc = go xs (10 * acc + x)

dec2int'' :: Fractional a => [a] -> a
dec2int'' xs = sum [x * pow 10 w | (x, w) <- zip xs (reverse [0 .. length xs - 1])]

fromDigits :: [Integer] -> Integer
fromDigits = foldl addDigit 0
  where
    addDigit num d = 10 * num + d

fromDigits' :: [Integer] -> Integer
fromDigits' = foldl ((+) . (* 10)) 0

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(a, b) -> f a b
