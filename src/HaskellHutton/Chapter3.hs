module HaskellHutton.Chapter3 where

import Data.Void

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

add' :: Num a => a -> a -> a
add' x y = x + y

addOne = (+) 1

absurd' :: (Num a) => Void -> a
absurd' x = 1

-- Exercises

ans' :: [Char]
ans' = ['a', 'b', 'c']

ans'' :: (Char, Char, Char)
ans'' = ('a', 'b', 'c')

ans''' :: [(Bool, Char)]
ans''' = [(False, '0')]

ans'''' :: ([Bool], [Char])
ans'''' = ([False, True], ['0', '1'])

ans''''' :: [[a] -> [a]]
ans''''' = [tail, init, reverse]

bools = [True]

nums = [[1]]

add'' x y z = x + y + z

copy x = (x, x)

apply f a = f a
