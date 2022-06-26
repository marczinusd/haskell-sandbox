module HaskellBook.Chapter6 where

import Data.List
import GHC.Base ()
import GHC.Real (fromIntegral)

data Trivial
    = Trivial'
    | Cica

instance Eq Trivial where
    Trivial' == Trivial' = True
    Cica == Trivial' = False
    Trivial' == Cica = False
    Cica == Cica = True

instance Show Trivial where
    show Trivial' = "Trivial"
    show Cica = "Cica"

instance Ord Trivial where
    (<=) Trivial' Cica = True
    (<=) Cica Trivial' = False
    (<=) Cica Cica = (==) Cica Cica
    (<=) Trivial' Trivial' = (==) Trivial' Trivial'

data DayOfWeek
    = Mon
    | Tue
    | Weds
    | Thu
    | Fri
    | Sat
    | Sun

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

data Date = Date DayOfWeek Int

instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'

-- Exercises: Eq Instances
newtype TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn integer) (TisAn integer') = integer == integer'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two first second) (Two first' second') =
        first == first' && second == second'

data StringOrInt
    = TisAnInt Int
    | TisAString String
    deriving (Show)

instance Eq StringOrInt where
    (==) (TisAnInt int) (TisAnInt int') = int == int'
    (==) (TisAString str) (TisAString str') = str == str'
    (==) _ _ = False

data Pair a = Pair a a
    deriving (Show)

instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b = Tuple a b
    deriving (Show)

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a
    = ThisOne a
    | ThatOne a
    deriving (Show)

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

data EitherOr a b
    = Hello a
    | Goodbye b
    deriving (Show)

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'
    (==) _ _ = False

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX :: Int
myX = 1 :: Int

sigmund' :: Int -> Int
sigmund' x = myX

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB i a = aToB a + fromIntegral i
