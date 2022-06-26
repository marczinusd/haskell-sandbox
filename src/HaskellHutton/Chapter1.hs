module HaskellHutton.Chapter1 (
    hello,
    sum',
    double,
    qsort,
    seqn,
    product',
    qsortRev,
    qsortUniq,
) where

hello :: [Char]
hello = "hello world"

double :: Num a => a -> a
double x = x + x

sum' :: (Foldable t, Num b) => t b -> b
sum' = foldr (+) 0

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

seqn [] = return []
seqn (act : acts) = do
    x <- act
    xs <- seqn acts
    return (x : xs)

-- Exercises
product' :: Num a => [a] -> a
product' = foldr (*) 1

qsortRev :: Ord a => [a] -> [a]
qsortRev [] = []
qsortRev (x : xs) = qsortRev larger ++ [x] ++ qsortRev smaller
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b >= x]

qsortUniq :: Ord a => [a] -> [a]
qsortUniq [] = []
qsortUniq (x : xs) = qsortUniq smaller ++ [x] ++ qsortUniq larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]
