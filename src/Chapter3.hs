module Chapter3 where

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
  where
    woot :: Integer
    woot = 10

topLevelValue :: Integer
topLevelValue = 5

area d = pi * (r * r)
  where
    r = d / 2

thirdLetter :: String -> Char
thirdLetter x = x !! 2

rvrs :: String
rvrs = drop 9 x ++ " " ++ take 2 (drop 6 x) ++ " " ++ take 5 x
  where
    x = "Curry is awesome"

mult :: Num a => a -> (a -> (a -> a))
mult = \x -> \y -> \z -> x * y * z

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a :: (a -> c) -> a -> a
a aToC a = a

a' :: (a -> b) -> a -> b
a' aToB = aToB

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
sing = if x > y
       then fstString x
       else sndString y
  where
    x = "Singin"

    y = "Somewhere"

main :: IO ()
main = do
  print $ 1 + 2
  print 10
  print (negate $ -1)
  print ((+) 0 blah)
  where
    blah = negate 1

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g $ f x

data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWz x = fst $ yToWz (xToY x)
