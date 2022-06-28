module HaskellHutton.Chapter8 where

import HaskellHutton.Chapter7 (int2bin)

-- Type aliases
type Pos = (Int, Int)
type Trans = Pos -> Pos

type Pair a = (a, a)
type Assoc k v = [(k, v)]

-- Union types
--data Bool' = False | True

data Move = North | South | West | East
    deriving (Show, Eq)

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move West (x, y) = (x - 1, y)
move East (x, y) = (x + 1, y)

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Rect n m) = n * m
area (Circle r) = pi * r ^ 2

data Maybe' a = Nothing' | Just' a

safeDiv :: Int -> Int -> Maybe' Int
safeDiv _ 0 = Nothing'
safeDiv m n = Just' (m `div` n)

-- Newtypes

-- newtype Nat = Nat Int
--     deriving (Eq, Show)

data Nat = Zero | Succ Nat
    deriving (Show)

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ x) = 1 + natToInt x

intToNat :: Int -> Nat
intToNat x
    | x == 0 = Zero
    | otherwise = Succ (intToNat (x - 1))

add :: Nat -> Nat -> Nat
add x y = intToNat (natToInt x + natToInt y)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Integer
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

depthFirst :: Tree a -> [a]
depthFirst (Leaf x) = [x]
depthFirst (Node left val right) = [val] ++ depthFirst left ++ depthFirst right

breadthFirst :: Tree a -> [a]
breadthFirst (Leaf x) = [x]
breadthFirst (Node left val right) = val : go [left, right]
  where
    go [] = []
    go [x] = visit x ++ go (nodesToAppend x)
    go nodesToVisit = visit (head nodesToVisit) ++ go (tail nodesToVisit ++ nodesToAppend (head nodesToVisit))
    visit (Leaf x) = [x]
    visit (Node _ val _) = [val]
    nodesToAppend (Leaf _) = []
    nodesToAppend (Node left _ right) = [left, right]

class Bla a where
    (===), (/==) :: a -> a -> Bool
    x /== y = not (x === y)

-- Tautology checker
data Prop
    = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n -1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- Abstract machine

data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add n m) = value n + value m

data Op = EVAL Expr | ADD Int
type Cont = [Op]

eval' :: Expr -> Cont -> Int
eval' (Val n) c = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

value' :: Expr -> Int
value' e = eval' e []

-- value (Add (Add (Val 2) (Val 3)) (Val 4))
x :: Int
x = value (Add (Add (Val 2) (Val 3)) (Val 4))
