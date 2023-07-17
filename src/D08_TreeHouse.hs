module D08_TreeHouse where

import Prelude as P
import Data.Char
import Data.Sequence as Seq hiding (filter) 

data Forest a = Forest (Int,Seq (Tree a))
    deriving Show

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

forestSideLength :: Int -> Int
forestSideLength l = isqrt l

slen :: (Forest a) -> Int
slen (Forest a) = (\(x,_) -> x) a

trees :: (Forest a) -> Seq (Tree a)
trees (Forest a) = (\(_,x) -> x) a

buildForest :: [Char] -> (Forest a)
buildForest uinput = (Forest (sl,Seq.fromList treelst))
    where
        str = Seq.fromList (filter (\x -> x/='\n') uinput)
        len = Seq.length str
        sl = forestSideLength (P.length str)
        treelst = [ (createTree (toCart i sl) sl (Seq.index str i)) | i <- [0 .. (len-1)]]

createTree :: (Int,Int) -> Int -> Char -> (Tree a)
createTree p sidelen c = Tree (p,digitToInt c,isEdge p sidelen)

getTree :: (Int,Int) -> (Forest a) -> (Tree a)
getTree coord fr  = Seq.index (trees fr) i
    where i = toIndex coord (slen fr)

updateTree :: (Tree a) -> (Int,Int) -> (Forest a) -> (Forest a)
updateTree t coord fr = Forest (slen fr,(Seq.update i t (trees fr)))
    where i = toIndex coord (slen fr)

isEdge :: (Int,Int) -> Int -> Bool
isEdge (x,y) sidelen | edge x || edge y = True
                     | otherwise = False
    where
        edge a = (a==0 || a==(sidelen-1))

toCart :: Int -> Int -> (Int,Int)
toCart i sl = 
    let x = i `mod` sl
        y = i `div` sl
    in (x,y)

left :: (Int,Int) -> (Int,Int)
left (x,y) = (x-1,y)

up :: (Int,Int) -> (Int,Int)
up (x,y) = (x,y-1)

down :: (Int,Int) -> (Int,Int)
down (x,y) = (x,y+1)

right :: (Int,Int) -> (Int,Int)
right (x,y) = (x+1,y)

toIndex :: (Int,Int) -> Int -> Int
toIndex (x,y) sl = y * sl + x 

data Tree a = Tree ((Int,Int),Int,Bool)
    deriving Show

smaller :: (Tree a) -> (Tree a) -> Bool
smaller t other = height t < height other 

isVisible :: (Tree a) -> Bool 
isVisible (Tree a) = (\(_,_,x) -> x) a 

setVisible :: (Tree a) -> (Tree a)
setVisible t = if isVisible t
               then t
               else (Tree (pos t,height t, True))

height :: (Tree a) -> Int
height (Tree a) = (\(_,x,_) -> x) a

pos :: (Tree a) -> (Int,Int)
pos (Tree a) = (\(x,_,_) -> x) a

treeLeftOf :: (Tree a) -> (Forest a) -> (Tree a)
treeLeftOf t fr = (Seq.index (trees fr) i)
    where i = toIndex (left (pos t)) (slen fr)

solveVis :: (Forest a) -> (Forest a)
solveVis fr 
    where
        [ i | i <- [0 .. (Seq.length (trees fr))-1], isVisible (getTree (toCart i (slen fr)) fr) == False] 

path :: [Char]
path = "input/treetop_tree_house_exp.txt"

solve :: IO ()
solve = do
    uinput <- readFile path
    let fr = buildForest uinput 
    let solving = [ i | i <- [0 .. (Seq.length (trees fr))-1], isVisible (getTree (toCart i (slen fr)) fr) == False]
    print solving 
