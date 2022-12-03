module D01_CalourieCounting where
import Data.List
import Data.List.Split
import System.Environment
import Control.Monad

calData :: IO ()
calData = do
    udata <- readFile "input/calorie_counting.txt"
    let cals = toInts udata
    let sumCals = map sum cals
    let maxCals = maximum sumCals
    let totalTopThree = sum (take 3 (reverse (sort sumCals)))
    print maxCals
    print totalTopThree 

toInts :: String -> [[Int]]
toInts d = map (map read) splitting :: [[Int]]
    where
        splitting = map lines (splitOn "\n\n" d)
