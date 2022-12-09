module D06_TuningTrouble where

import Data.List

path :: [Char]
path = "input/tuning_trouble.txt"

solve :: IO ()
solve = do
    uinput <- readFile path
    let bufferlst = lines uinput
    let packetMark = map (\s -> (checkMarker 4 s)) bufferlst
    let msgMark = map (\s -> (checkMarker 14 s)) bufferlst
    print packetMark
    print msgMark 

hasUnique :: [Char] -> Bool
hasUnique xs = length xs == length (uniqueElems xs)
    where
        uniqueElems [] = []
        uniqueElems (y:ys) = y:uniqueElems (filter ((/=) y) ys)

checkMarker :: Int -> [Char] -> Int
checkMarker len str = check 0 "" str
    where 
        check n _ "" = n
        check n r (x:xs) | less && unique = check (n+1) (x:r) xs
                         | eq && unique = (n+1)
                         | otherwise = check (n+1) (init (x:r)) xs
            where 
                less   = length (x:r) < len
                eq     = length (x:r) == len
                unique = hasUnique (x:r) 

