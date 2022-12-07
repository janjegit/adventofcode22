module D05_SupplyStacks where

import Data.List.Split
import qualified Data.Sequence as Seq

cargohold :: [[Char]] -> Seq.Seq ([Char])
cargohold lst = Seq.fromList lst

readInstr :: [[Char]] -> [(Int,Int,Int)]
readInstr lst = map toTri lst 

toTri :: [Char] -> (Int,Int,Int)
toTri rawInstruction = (getN 0, getN 1, getN 2)
    where
        splitInstr = splitOn "," rawInstruction 
        toInt x = read x :: Int
        getN n = toInt (head (drop n splitInstr))

move :: Int -> (Int,Int,Int) -> Seq.Seq ([Char]) -> Seq.Seq ([Char])
move _ (0,_,_) cargo = cargo
move num (repeats,from,to) cargo = 
    move num (repeats-1,from,to) (Seq.update nfrom start (Seq.update nto dest cargo)) 
    where
        nfrom = (from - 1)
        nto = (to - 1)
        stack i = Seq.index cargo i
        dest  = (take num (stack (nfrom))) ++ (stack nto)
        start = drop num (stack (nfrom))

work :: [(Int,Int,Int)] -> Seq.Seq ([Char]) -> Seq.Seq ([Char])
work [] cargo = cargo
work instructions cargo = work (drop 1 instructions) (move 1 instruction cargo)
    where instruction = head instructions

workfast :: [(Int,Int,Int)] -> Seq.Seq ([Char]) -> Seq.Seq ([Char])
workfast [] cargo = cargo
workfast instructions cargo = workfast (drop 1 instructions) (move num changedRep cargo)
    where 
        instruction = (head instructions)
        num = (\(x,_,_) -> x) instruction
        changedRep = (\(_,y,z) -> (1,y,z)) instruction

getTops :: Seq.Seq ([Char]) -> [Char]
getTops Seq.Empty = ""
getTops cargo = (head (Seq.index cargo 0)) : getTops (Seq.drop 1 cargo)

path :: String
path = "input/supply_stacks.txt"

solve :: IO ()
solve = do
    finput <- readFile path
    let splitting = splitOn "\n\n" finput
    let instructions = readInstr (lines $ last splitting)
    let cargo = cargohold (map reverse (lines $ head splitting))
    print (getTops (work instructions cargo))
    print (getTops (workfast instructions cargo))
