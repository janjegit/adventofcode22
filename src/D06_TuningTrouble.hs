module D06_TuningTrouble where

import Data.List

path :: [Char]
path = "input/tuning_trouble.txt"

solve :: IO ()
solve = do
    uinput <- readFile path
    let bufferlst = lines uinput
    let markers = map checkMarker bufferlst
    print markers

hasUnique :: [Char] -> Bool
hasUnique xs = length xs == length (uniqueElems xs)
    where
        uniqueElems [] = []
        uniqueElems (y:ys) = y:uniqueElems (filter ((/=) y) ys)

checkMarker :: [Char] -> Int
checkMarker str = check 0 "" str
    where 
        check n _ "" = n
        check n r (x:xs) | less && unique = check (n+1) (x:r) xs
                         | eq && unique = (n+1)
                         | otherwise = check (n+1) (init (x:r)) xs
            where 
                less   = length (x:r) < 4
                eq     = length (x:r) == 4
                unique = hasUnique (x:r) 

