{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11 where

import           GHC.Unicode (toUpper)
import           Chapter9 (capitalize)

data PugType = PugData

data HuskyType a = HuskyData

newtype DogueDeBordeaux doge = DogueDeBordeaux doge

data Hu

data Doggies a = Husky a
               | Mastiff a
  deriving (Eq, Show)

x :: doge -> DogueDeBordeaux doge
x = DogueDeBordeaux

y :: DogueDeBordeaux Integer
y = x 1

newtype Price = Price Integer
  deriving (Eq, Show)

x' :: Price
x' = Price 1

data Manufacturer = Mini
                  | Mazda
                  | Tata
  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline
  deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _) = False

isPlane :: Vehicle -> Bool
isPlane (Car _ _) = False
isPlane (Plane _) = True

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car manu _) = Just manu
getManu _ = Nothing

newtype Goats = Goats Int
  deriving (Eq, Show, TooMany)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (a, b) = a > 42

instance TooMany (Int, Int) where
  tooMany (a, b) = (a + b) > 42

data Person = Person { name :: String, age :: Int }
  deriving (Eq, Show)

papu = Person "Papu" 55

papuAge = age papu

papuName = name papu

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
  deriving (Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
  deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = x, lang = y }
                 | x <- allOperatingSystems
                 , y <- allLanguages]

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node Leaf a Leaf) = [a]
preorder (Node Leaf a right) = a:preorder right
preorder (Node left a Leaf) = a:preorder left
preorder (Node left a right) = a:preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node Leaf a Leaf) = [a]
inorder (Node Leaf a right) = a:inorder right
inorder (Node left a Leaf) = a:inorder left
inorder (Node left a right) = inorder left ++ a:inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node Leaf a Leaf) = [a]
postorder (Node Leaf a right) = a:postorder right
postorder (Node left a Leaf) = a:postorder left
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node Leaf a Leaf) = f a acc
foldTree f acc (Node Leaf a right) = f a (foldTree f acc right)
foldTree f acc (Node left a Leaf) = f a (foldTree f acc left)
foldTree f acc (Node left a right) =
  foldTree f (f a (foldTree f acc right)) left

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] y = True
isSubseqOf _ [] = False
isSubseqOf a@(x:xs) (y:ys) = x == y && isSubseqOf xs ys || isSubseqOf a ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords word@(x:xs) =
  if x /= ' '
  then (takeWhile (/= ' ') word, capitalize $ takeWhile (/= ' ') word)
    :capitalizeWords (dropWhile (/= ' ') word)
  else capitalizeWords xs
  where
    capitalize "" = ""
    capitalize (x:xs) = toUpper x:xs

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
               then putStrLn "Preorder yep"
               else putStrLn "Preorder nope"

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
              then putStrLn "Inorder yep"
              else putStrLn "Preorder nope"

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
                then putStrLn "Postorder yep"
                else putStrLn "Preorder nope"

mapOkay :: IO ()
mapOkay = if mapTree (+ 1) testTree' == mapExpected
          then putStrLn "mapOkay yep"
          else putStrLn "mapOkay nope"

testFoldTree :: IO ()
testFoldTree = if foldTree (+) 0 testTree == 6
               then putStrLn "testFoldTree yep"
               else putStrLn "testFoldTree nope"

main :: IO ()
main = do
  mapOkay
  testPreorder
  testInorder
  testPostorder
  testFoldTree
