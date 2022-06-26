module HaskellBook.Chapter12 where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

split :: String -> [String]
split (' ' : xs) = split $ dropWhile (== ' ') xs
split word@(x : xs) = takeWhile (/= ' ') word : split (dropWhile (/= ' ') xs)
split "" = []

replaceThe :: String -> String
replaceThe ('t' : 'h' : 'e' : xs) = replaceThe ("a" ++ xs)
replaceThe (x : xs) = x : replaceThe xs
replaceThe "" = ""

vowels :: [Char]
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

vowelCount :: String -> Int
vowelCount s = length $ filter isVowel s

newtype Word' = Word' String
    deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord x =
    if vCount > ((length x) - vCount)
        then Nothing
        else Just (Word' x)
  where
    vCount = vowelCount x

data Nat
    = Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + natToInteger a

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x > 0 = Just (integerToNat' (x - 1))
    | x == 0 = Just (Succ Zero)
    | otherwise = Nothing

integerToNat' :: Integer -> Nat
integerToNat' x
    | x > 0 = Succ (integerToNat' (x - 1))
    | x == 0 = Succ Zero
    | otherwise = Zero

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ Nothing = b

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe (x : _) = Just x
listToMaybe [] = Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes ((Just a) : xs) = a : catMaybes xs
catMaybes (Nothing : xs) = catMaybes xs
catMaybes [] = []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs =
    if any isNothing xs
        then Nothing
        else Just (catMaybes xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
    Just (a, b) -> a : myUnfoldr f b
    Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\x -> Just (a, f a)) (f a)
