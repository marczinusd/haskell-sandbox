module HaskellBook.Chapter18 where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a

x = join $ putStrLn <$> getLine

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x * x, x * x]
        else [x * x]

data Cow = Cow {name :: String, age :: Int, weight :: Int}
    deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
     in if n == "Bess" && w > 499
            then Nothing
            else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
    noEmpty name'
        >>= \nammy ->
            noNegative age'
                >>= \agey ->
                    noNegative weight'
                        >>= \weighty -> weightCheck (Cow nammy agey weighty)

type Founded = Int

type Coders = Int

data SoftwareShop = Shop {founded :: Founded, programmers :: Coders}
    deriving (Eq, Show)

data FoundedError
    = NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers

data Sum a b
    = First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second a) = Second (f a)

instance Applicative (Sum a) where
    pure a = Second a

    First a <*> _ = First a
    _ <*> First a = First a
    Second a <*> Second b = Second (a b)

instance Monad (Sum a) where
    return = pure

    First a >>= _ = First a
    Second a >>= b = b a
