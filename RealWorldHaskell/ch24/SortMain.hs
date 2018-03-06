
module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randoms)

import Sorting

--testFunction = parSort
--testFunction = sort
--testFunction = sillySort
testFunction = parSort2 10

randomInts :: Int -> StdGen -> [Int]
randomInts k g =
    let result = take k (randoms g)
    in  force result `seq` result

main = do
    args <- getArgs
    let count
            | null args = 5000000
            | otherwise = read (head args)
    input <- randomInts count `fmap` getStdGen
    putStrLn $ "Length: " ++ show (length input)
    start <- getCurrentTime
    let sorted = testFunction input
    putStrLn $ "Length: " ++ show (length input) ++ " sorted"
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " slapsed"
