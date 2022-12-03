module D03_RucksackReorg where

import Data.Char
import Data.List.Split
import System.Environment
import Control.Monad

priority :: Char -> Int
priority c | isLower c = offset c 'a'
           | otherwise = offset c 'A' + offset 'z' 'a'
    where 
        offset ch from = (ord ch) - (ord from) + 1

findDublicateItem :: ([Char],[Char]) -> [Char]
findDublicateItem (fstCompart,sndCompart) = 
    take 1 [ x | x <- fstCompart, y <- sndCompart, x == y]

findGroupBatch :: ([Char],[Char],[Char]) -> [Char]
findGroupBatch (as,bs,cs) = 
    take 1 [x | x <- as, y <- bs, z <- cs, x == y && y == z] 

splitHalf :: [Char] -> ([Char],[Char])
splitHalf [] = ([],[])
splitHalf content = (take half content, drop half content)
    where half = (length content) `div` 2

path :: [Char]
path = "input/rucksack.txt"

sumDublicateItems :: IO ()
sumDublicateItems = do
    rucksacks <- readFile path 
    let contents = lines rucksacks
    let sumPriority = sum (map (priority . head) (map findDublicateItem (map splitHalf contents)))
    print sumPriority 

sumGroupBatches = do
    rucksacks <- readFile path 
    let contents = lines rucksacks
    let batches = map findGroupBatch (map (\(x:y:z:as)-> (x,y,z)) (chunksOf 3 contents))
    print (sum (map (priority . head) batches ))
