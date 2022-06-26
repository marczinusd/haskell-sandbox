module HaskellHutton.Chapter4 where

-- Conditionals
abs' :: (Ord p, Num p) => p -> p
abs' n = if n >= 0 then n else - n

signum' :: (Ord a, Num a, Num p) => a -> p
signum' n =
    if n < 0
        then -1
        else if n == 0 then 0 else 1

-- Guards
abs'' :: (Ord p, Num p) => p -> p
abs'' n
    | n >= 0 = n
    | otherwise = - n

signum'' :: (Ord a, Num a, Num p) => a -> p
signum'' n
    | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1

-- Pattern matching
not' :: Bool -> Bool
not' False = True
not' True = False

(&&&) :: Bool -> Bool -> Bool
True &&& True = True
True &&& False = False
False &&& True = False
False &&& False = False

(&&&&) :: Bool -> Bool -> Bool
True &&&& True = True
_ &&&& _ = False

-- Tuple patterns
fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, x) = x

-- List patterns
test :: [Char] -> Bool
test [_, 'a', _] = True
test _ = False

-- Lambda expressions
bla :: Integer
bla = (\x -> x + x) 2

-- Exercises
halve :: [a] -> ([a], [a])
halve xs
    | odd len = (xs, xs)
    | otherwise = (take half xs, take len $ drop half xs)
  where
    len = length xs
    half = len `div` 2

third :: [a] -> Maybe a
third xs
    | length xs >= 3 = Just $ xs !! 2
    | otherwise = Nothing

third' :: [a] -> Maybe a
third' xs
    | length xs >= 3 = Just $ head $ tail $ tail xs
    | otherwise = Nothing

third'' :: [a] -> Maybe a
third'' [_, _, x] = Just x
third'' _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : xs) = Just xs

luhnDouble :: Int -> Int
luhnDouble x
    | doubled > 9 = doubled - 9
    | otherwise = doubled
  where
    doubled = x + x

luhnDouble' :: Int -> Int -> Int -> Int -> Bool
luhnDouble' a b c d =
    sum [luhnDouble a, b, luhnDouble c, d] `mod` 10 == 0
