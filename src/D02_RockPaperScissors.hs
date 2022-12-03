module D02_RockPaperScissors where
import Data.List.Split
import System.Environment
import Control.Monad

data Shape = Rock | Paper | Scissors 
    deriving (Show, Eq, Ord)

data Result = Win | Draw | Lose
    deriving (Show, Eq)

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

decodeShape :: Char -> Shape
decodeShape c | c == 'A' = Rock
              | c == 'B' = Paper
              | otherwise = Scissors

decodeResult :: Char -> Result
decodeResult c | c == 'X' = Lose 
               | c == 'Y' = Draw
               | otherwise = Win 

totalScore :: [(Char,Char)] -> Int
totalScore [] = 0
totalScore xs = sum (map turnScore xs)

turnScore :: (Char,Char) -> Int
turnScore (a,b) = resultScore + shapeScore
    where
        opposingShape = decodeShape a
        desiredResult = decodeResult b
        playerShape   = chooseShape opposingShape desiredResult
        resultScore   = getScore (opposingShape `vs` playerShape)
        shapeScore    = getScore playerShape

chooseShape :: Shape -> Result -> Shape
chooseShape s desiredResult | desiredResult == Win  = chooseforWin s
                            | desiredResult == Lose = chooseforLose s
                            | otherwise             = chooseforDraw s

chooseforWin :: Shape -> Shape
chooseforWin opposingShape | opposingShape == Rock     = Paper
                           | opposingShape == Scissors = Rock
                           | otherwise                 = Scissors

chooseforLose :: Shape -> Shape
chooseforLose opposingShape | opposingShape == Paper    = Rock
                            | opposingShape == Rock     = Scissors
                            | otherwise                 = Paper

chooseforDraw :: Shape -> Shape
chooseforDraw opposingShape = opposingShape

scoreStratGuide :: IO ()
scoreStratGuide = do
    raw <- readFile "input/strategy_guide.txt"
    let guide = toChars raw 
    print (totalScore guide)

toChars :: String -> [(Char,Char)]  
toChars s = map (\(x:_:z:_) -> (x,z)) (lines s)
