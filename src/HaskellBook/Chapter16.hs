module HaskellBook.Chapter16 where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted
  :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

a :: [Int]
a = (+ 1) <$> read "[1]" :: [Int]

b :: Maybe [[Char]]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Integer -> Integer
c = (* 2) . (\x -> x - 2)

d :: Integer -> [Char]
d = ((return '1' ++) . show) . (\x -> [x, 1 .. 3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = (read <$> ("123" ++)) . show <$> ioi
    in (* 3) <$> changed

data Two a b = Two a b
  deriving (Eq, Show)

data Or a b = First a
            | Second b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

data Two' a b = Two' a b

instance Functor (Two' a) where
  fmap f (Two' a b) = Two' a (f b)

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

data Sum a b = First' a
             | Second' b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First' a) = First' a
  fmap f (Second' b) = Second' (f b)

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v
