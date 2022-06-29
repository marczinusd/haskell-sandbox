{-# LANGUAGE InstanceSigs #-}

module HaskellHutton.Chapter12Vid where

bla = (fmap . fmap) Just [[1, 2, 3, 4]]

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
    fmap f Nothing' = Nothing'
    fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
    pure = Just'
    Nothing' <*> _ = Nothing'
    (Just' f) <*> mx = fmap f mx

a1 :: Num a => Maybe (a -> a -> a -> a)
a1 = pure (\a b c d -> a + b + c + d) <*> Just 1

a2 :: Maybe (Integer -> Integer -> Integer)
a2 = a1 <*> Just 2

-- instance Applicative [] where
--     pure x = [x]
--     gs <*> xs = [g x | x <- xs, g <- gs]

data Expr = Val Int | Div Expr Expr

data Expr' = Val' Int | Div' Expr' Expr' | Mult' Expr' Expr' | Add' Expr' Expr'

eval :: Expr -> Int
eval (Val n) = n
eval (Div l r) = eval l `div` eval r

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv l r = Just (l `div` r)

safeEval :: Expr -> Maybe Int
safeEval (Val n) = Just n
safeEval (Div l r) =
    case safeEval l of
        Nothing -> Nothing
        Just a -> case safeEval r of
            Nothing -> Nothing
            Just b -> a `safediv` b

safeEval' :: Expr' -> Maybe Int
safeEval' (Val' n) = Just n
safeEval' (Mult' l r) = evalExpr safeMult l r
safeEval' (Add' l r) = evalExpr safeAdd l r
safeEval' (Div' l r) = evalExpr safediv l r

evalExpr f l r = safeEval' l >>= (\x -> safeEval' r >>= (\y -> x `f` y))

evalExpr' f l r = do
    x1 <- safeEval' l
    x2 <- safeEval' r
    return (x1 `f` x2)
safeMult x y = Just (x * y)
safeAdd x y = Just (x + y)

pairs :: Monad m => m a -> m b -> m (a, b)
pairs xs ys = do
    x <- xs
    y <- ys
    return (x, y)

newtype State a = S a
newtype ST a = St (State a -> (a, State a))

app :: ST a -> State a -> (a, State a)
app (St x) s = x s

instance Show a => Show (State a) where
    show (S a) = "State(" ++ show a ++ ")"

incrst :: ST Int
incrst = St (\(S a) -> (a + 1, S (a + 1)))

incrState :: Int -> State Int -> (Int, State Int)
incrState n (S x) = (x + n, S (x + n))
