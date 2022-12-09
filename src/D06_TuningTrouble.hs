module D06_TuningTrouble where

import Data.List

path :: [Char]
path = "input/tuning_trouble_exp.txt"

solve :: IO ()
solve = do
    uinput <- readFile path
    print uinput

hasUnique :: [Char] -> Bool
hasUnique xs = length xs == length (uniqueElems xs)

uniqueElems :: [Char] -> [Char] 
uniqueElems [] = []
uniqueElems (x:xs) = x:uniqueElems (filter ((/=) x) xs)
