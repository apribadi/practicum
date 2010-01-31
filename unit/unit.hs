module Main where

import Control.Monad
import Data.List

solve :: [(Float, String, String)] -> (String, String) -> String
solve conversions task = "ASdf"

readConversion :: IO (Float, String, String)
readConversion = do
    [n, unit1, unit2] <- liftM words $ getLine
    return (read n, unit1, unit2)

readTask :: IO (String, String)
readTask = do
    [unit1, unit2] <- liftM words $ getLine
    return (unit1, unit2)

main :: IO ()
main = do 
    [n, m] <- liftM (map read . words) $ getLine :: IO [Int]
    conversions <- sequence . replicate n $ readConversion
    tasks <- sequence . replicate m $ readTask
    sequence_ $ map (putStrLn . solve conversions) tasks

