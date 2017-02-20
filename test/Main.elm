module Main exposing (add)


add : Int -> Int -> Int
add =
    addHelper


addHelper : Int -> Int -> Int
addHelper =
    (+)
