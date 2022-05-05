module HaskellBook.Chapter17 where

import           Data.List (elemIndex)

added :: Maybe Integer
added = fmap (+ 3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

xs :: [Integer]
xs = [1, 2, 3]

ys :: [Integer]
ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x' <*> y'')

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure f = Identity f

  Identity f <*> Identity a = Identity (f a)

bla = const <$> Just "Hello" <*> Just "World"

bla' = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]

bla'' = pure (.) <*> [(+ 1)] <*> [(* 2)] <*> [1, 2, 3]

