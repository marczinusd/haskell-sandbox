{-# LANGUAGE InstanceSigs #-}

module HaskellHutton.Chapter12 where

-- Functors
inc :: [Int] -> [Int]
inc [] = []
inc (x : xs) = (x + 1) : inc xs

sqr :: [Int] -> [Int]
sqr [] = []
sqr (x : xs) = (x ^ 2) : sqr xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

inc' :: [Integer] -> [Integer]
inc' = map' (+ 1)

sqr' :: [Integer] -> [Integer]
sqr' = map' (^ 2)

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
    fmap' = map

data Maybe' a = Nothing' | Just' a

instance Functor' Maybe' where
    fmap' f (Just' a) = Just' (f a)
    fmap' f Nothing' = Nothing'

class Monad' m where
    return' :: a -> m a
    bind' :: (a -> m b) -> m a -> m b

instance Monad' Maybe' where
    return' a = Just' a
    bind' f (Just' a) = f a
    bind' _ Nothing' = Nothing'

instance Functor' IO where
    fmap' g mx = do
        x <- mx
        return (g x)

-- Applicatives

fmap0 :: Applicative f => a -> f a
fmap0 = pure

fmap1 :: Applicative f => (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 f g x = pure f <*> g <*> x

-- Monads

type State' = Int

-- type ST' = State' -> State'
-- type ST' a = State' -> (a, State')
newtype ST' a = S (State' -> (a, State'))
app :: ST' a -> State' -> (a, State')
app (S st) x = st x

instance Functor ST' where
    fmap :: (a -> b) -> ST' a -> ST' b
    fmap f st = S (\s -> let (x, s') = app st s in (f x, s'))
