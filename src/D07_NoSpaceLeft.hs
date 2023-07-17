module D07_NoSpaceLeft where

import Data.List.Split

type Foldername = String
type Filename = String

data DirTree = File Filename Int | Folder Foldername [DirTree]
    deriving Show

testdir :: DirTree
testdir = 
    Folder "/" [Folder "a" [Folder "e" [File "i" 584],File "b.txt" 14848514, File "c.txt" 8504156],Folder "d" [File "j" 4060174, File "d.log" 8033020, File "d.ext" 5626152]]

testdir2 :: DirTree
testdir2 = Folder "e" [File "i" 5, File "j" 400, Folder "a" [File "i" 5]] 

dirSize :: DirTree -> Int
dirSize (File _ size) = size
dirSize (Folder _ entries) = sum (map dirSize entries)

cd :: DirTree -> Foldername -> DirTree
cd tree foldername = undefined 

getName :: [Char] -> [Char] 
getName d = last (splitOn " " d)

parseFile :: [Char] -> DirTree 
parseFile s = (File name size)
    where 
        name = last (splitOn " " s)
        size = read (head (splitOn " " s)) :: Int

parseEntry :: [Char] -> DirTree
parseEntry entry = if isDir entry
                        then Folder (getName entry) []
                   else parseFile entry
    where
        isDir s = (take 3 s) == "dir"

path :: [Char]
path = "input/no_space_left_exp.txt"

solve :: IO ()
solve = do
    uinput <- readFile path
    print (lines uinput)

