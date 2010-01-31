module Main 
  (
    main
  ) where

import Data.Array (listArray, (!))
import Control.Monad (liftM)

data Line = Line Int Int Int

-- Helper functions
count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs

-- Solve
solve :: Int -> [Line] -> Int
solve r lines = 
    let n = length lines
        a = listArray (0, n-1) lines in
    count (uncurry $ meet r) [(a!i, a!j) | i <- [0..n-1], j <- [0..i-1]]

-- Check if two lines meet within raduis r of the origin.
meet :: Int -> Line -> Line -> Bool
meet r line1 line2 = 
    let Line a b e = line1
        Line c d f = line1 in  
    (e*d - b*f)^2 + (a*f - e*c)^2 <= r^2 * (d*b - b*c)^2

-- I/O procedure
main :: IO ()
main = do  
    (n, r) <- readLine parInts
    lines <- sequence . replicate n $ readLine parThread
    print $ solve r lines 
      where 
        readLine par = return . par . words =<< getLine
        parInts (n:r:_) = (read n, read r)
        parThread (a:b:c:_) = Line (read a) (read b) (-(read c))

