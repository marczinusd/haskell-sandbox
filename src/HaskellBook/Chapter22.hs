module HaskellBook.Chapter22 where

import Control.Applicative
import Data.Char

boop :: Integer -> Integer
boop = (* 2)

doop :: Integer -> Integer
doop = (+ 10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = (rev . cap)

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev
