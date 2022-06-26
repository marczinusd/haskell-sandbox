module HaskellHutton.Chapter6 where

fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact n - 1

-- Exercises
