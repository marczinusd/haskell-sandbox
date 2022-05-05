module HaskellBook.Chapter8 where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

x = frappe "1"

x' = frappe (appedCatty "2")

x'' = appedCatty (frappe "blue")

x''' = cattyConny (frappe "pink")

x'''' = cattyConny "green" (appedCatty "blue")

x''''' = cattyConny (flippy "Pugs" "are") "awesome"

mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))
