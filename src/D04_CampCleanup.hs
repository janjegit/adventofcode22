module D04_CampCleanup where

import Data.List.Split
import qualified Data.Set as Set

path :: [Char]
path = "input/camp_cleanup.txt"

solve :: IO ()
solve = do
    finput <- readFile path
    let schedList = schedules finput
    print (sumOccOf True (findInPairs containsOther schedList))
    print (sumOccOf True (findInPairs containsOverlap schedList))

sumOccOf :: Eq a => a -> [a] -> Int
sumOccOf a xs = sum [ 1 | x <- xs, x == a]

schedules :: String -> [(Int,Int)]
schedules string = concat (map pairSingleSchedule (lines string))

pairSingleSchedule :: [Char] -> [(Int,Int)]
pairSingleSchedule string = (tupleOf fstHalf):(tupleOf sndHalf):[] 
    where
        cleanString = splitOneOf ",-" string
        fstHalf = take 2 cleanString
        sndHalf = drop 2 cleanString
        tupleOf half = (read (head half) :: Int, read (last half) :: Int)

containsOther :: (Int,Int) -> (Int,Int) -> Bool
containsOther a b = (fstInSnd a b) || sndInFst || a == b
    where 
        fstInSnd x y = (x /= y) && ((fst x) <= (fst y) && (snd x) >= (snd y))
        sndInFst = fstInSnd b a

containsOverlap :: (Int,Int) -> (Int,Int) -> Bool
containsOverlap a b = not (Set.disjoint (range a) (range b))
    where
        range x = Set.fromList [(fst x) .. (snd x)]

findInPairs :: ((Int,Int) -> (Int,Int) -> Bool) -> [(Int,Int)] -> [Bool]
findInPairs f [] = []
findInPairs f xs = check (take 2 xs) : findInPairs f (drop 2 xs)
    where
        check ys = f (head ys) (last ys)
