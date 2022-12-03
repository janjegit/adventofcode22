module D02_RockPaperScissors where
import Data.List.Split
import System.Environment
import Control.Monad

data Shape = Rock | Paper | Scissors 
    deriving (Show, Eq, Ord)

data Result = Win | Draw | Lose
    deriving Show

class Score a where
    getScore :: a -> Int

instance Score Shape where
    getScore Rock     = 1
    getScore Paper    = 2
    getScore Scissors = 3

instance Score Result where
    getScore Win      = 6
    getScore Draw     = 3
    getScore Lose     = 0

vs :: Shape -> Shape -> Result
vs opponent you | opponent == Scissors && you == Rock     = Win 
                | opponent == Rock     && you == Scissors = Lose
                | opponent < you = Win
                | opponent > you = Lose
                | otherwise = Draw
{-
vs opponent you | opponent == Paper    && you == Scissors = Win
                | opponent == Scissors && you == Paper    = Lose
                | opponent == Scissors && you == Rock     = Win 
                | opponent == Rock     && you == Scissors = Lose
                | opponent == Rock     && you == Paper    = Win
                | opponent == Paper    && you == Rock     = Lose
                | otherwise = Draw
-}

decode :: Char -> Shape
decode 'A' = Rock
decode 'B' = Paper
decode 'C' = Scissors
decode 'X' = Rock
decode 'Y' = Paper
decode 'Z' = Scissors

totalScore :: [(Char,Char)] -> Int
totalScore [] = 0
totalScore xs = sum (map turnScore xs)

turnScore :: (Char,Char) -> Int
turnScore (a,b) = resultScore + shapeScore
    where
        opposingShape = decode a
        playerShape   = decode b
        resultScore   = getScore (opposingShape `vs` playerShape)
        shapeScore    = getScore playerShape
    
scoreStratGuide :: IO ()
scoreStratGuide = do
    raw <- readFile "input/strategy_guide.txt"
    let guide = toChars raw 
    print (totalScore guide)

toChars :: String -> [(Char,Char)]  
toChars s = map (\(x:y:z:xs) -> (x,z)) (lines s)
