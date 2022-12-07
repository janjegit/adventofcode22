module D05_SupplyStacks where

import Data.Maybe
import Data.List.Split
import qualified Data.Sequence as Seq

data Stack a = Empty | Stack a (Stack a)
    deriving Show

top :: Stack a -> Maybe a
top Empty = Nothing
top (Stack a s) = Just a

pop :: Stack a -> Stack a
pop Empty = Empty
pop (Stack a s) = s

push :: a -> Stack a -> Stack a
push a Empty = Stack a Empty
push a s = Stack a s

build :: [Char] -> Stack Char
build xs = build' (reverse xs) Empty 
    where
        build' [] s = s
        build' (y:ys) s = build' ys (push y s)

buildCargoHold :: [[Char]] -> Seq.Seq (Stack Char)
buildCargoHold lst = buildHold (reverse lst)
    where
        buildHold [] = Seq.Empty
        buildHold (x:xs) = (Seq.|>) (buildHold xs) (build (reverse x))

move :: Int -> Int -> Seq.Seq (Stack Char) -> Seq.Seq (Stack Char)
move from to sequ = if crate == Nothing
                        then sequ
                   else place
    where
        crate  = top (Seq.index sequ from)
        remove = Seq.update from (pop (Seq.index sequ from)) sequ
        place  = Seq.update to (push (fromJust crate) (Seq.index sequ to)) remove

work :: Seq.Seq (Stack Char) -> [(Int,Int,Int)] -> Seq.Seq (Stack Char)
work sequ instrLst = work' sequ instrLst
    where
        work' s [] = s
        work' s (x:xs) = work (instr sequ x) xs
        instr s (0,_,_) = s
        instr s (reps,from,to) = instr (move (from-1) (to-1) s) ((reps-1),from,to)

readInstr :: [[Char]] -> [(Int,Int,Int)]
readInstr lst = map toTri lst 

toTri :: [Char] -> (Int,Int,Int)
toTri rawInstruction = (getN 0, getN 1, getN 2)
    where
        splitInstr = splitOn "," rawInstruction 
        toInt x = read x :: Int
        getN n = toInt (head (drop n splitInstr))

eachTop :: Seq.Seq (Stack Char) -> [Char]
eachTop Seq.Empty = [] 
eachTop sequ = (contents sequ) : (eachTop (Seq.drop 1 sequ))
    where
        topCrateOf s = top (Seq.index s 0)
        contents s = if (topCrateOf s) == Nothing
                        then 'x'
                     else fromJust (topCrateOf s)


path :: String
path = "input/supply_stacks.txt"

solve :: IO ()
solve = do
    finput <- readFile path
    let splitting = splitOn "\n\n" finput
    let instructions = readInstr (lines $ last splitting)
    let cargo = buildCargoHold (lines $ head splitting)
    print (eachTop (work cargo instructions))

cargolist :: [[Char]]
cargolist = ["NZ","DCM","P"]
instrlist :: [(Int,Int,Int)]
instrlist = [(1,2,1),(3,1,3),(2,2,1),(1,1,2)]
rawInstrlist :: [[Char]]
rawInstrlist = ["1,2,1","3,1,3","2,2,1","1,1,2"]
