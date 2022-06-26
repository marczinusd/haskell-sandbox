{-# LANGUAGE TupleSections #-}

module HaskellBook.Chapter15 where

import Data.Monoid (Sum (Sum))

class Monoid' m where
    mempty' :: m
    mappend' :: m -> m -> m
    mconcat' :: [m] -> m
    mconcat' = foldr mappend' mempty'

-- List
x = mappend [1, 2, 3] [4, 5, 6]

-- [1,2,3,4,5,6]
x' = mappend (Sum 1) (Sum 5)

newtype Bla = Bla Int

data Optional a
    = Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
    Nada <> Nada = Nada
    Nada <> (Only a) = Only a
    (Only a) <> Nada = Only a
    (Only a) <> (Only a') = Only (a <> a')

newtype Mem s a = Mem {runMem :: s -> (a, s)}

combine :: Semigroup a => (b1 -> (a, b2)) -> (p -> (a, b1)) -> p -> (a, b2)
combine f g p = (a <> a', b2)
  where
    (a, b1) = g p

    (a', b2) = f b1

instance (Semigroup a) => Semigroup (Mem s a) where
    (<>) Mem{runMem = f} Mem{runMem = g} = Mem $ combine f g

instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)

    mappend = (<>)
