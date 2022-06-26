module HaskellMooc.Lecture1 where

hello = "hello world"

data Dog a = Dog a a a

x = Dog 1

y = x 2 3

y1 = Just 1

mapY (Just x) f = f x
mapY Nothing _ = Nothing

x1 =
    [ whole
    | first <- ["Eva", "Mike"]
    , last <- ["Smith", "Wood", "Odd"]
    , let whole = first ++ last
    , even (length whole)
    ]
