import Test.HUnit

module Print1 where

main :: IO ()
main = do
    putStrLn "hello world!"
    let x = "hello" ++ " " ++ "world!"
    putStrLn x
