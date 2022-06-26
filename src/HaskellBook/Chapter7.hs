module HaskellBook.Chapter7 where

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

functionC :: Ord p => p -> p -> p
functionC x y = case x > y of
    True -> x
    False -> y

ifEvenAdd2 :: Integral p => p -> p
ifEvenAdd2 n = case even n of
    True -> n + 2
    False -> n

nums :: (Ord a, Num a, Num p) => a -> p
nums x = case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

map2 :: (a -> b) -> [a] -> [b]
map2 = undefined

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.7 = 'C'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
  where
    y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
    | xs == reverse xs = True
    | otherwise = False

numbers :: (Ord a, Num a, Num p) => a -> p
numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1

dot :: (a -> b) -> (b -> c) -> (a -> c)
dot aToB bToC a = bToC $ aToB a

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10

    d = xLast `mod` 10

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y b
    | not b = x
    | b = y
    | otherwise = x

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)
