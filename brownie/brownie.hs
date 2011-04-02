module Main 
  (
    main
  ) where

import Data.Array (listArray, (!))
import Data.List (foldl')
import Control.Monad (liftM)

data Point = Point Int Int

score :: [[Int]] -> (Int, Int)
score xss = 
    let points = map (\[a,b] -> Point a b) xss
        n = length points
        mid = (n - 1) `div` 2
        Point a b = points!!mid
        scorePoint (Point x y) = (
            if ((x > a && y > b) || (x < a && y < b)) then 1 else 0,
            if ((x < a && y > b) || (x > a && y < b)) then 1 else 0
            )
        f old pt = let inc = scorePoint pt in
            (fst old + fst inc, snd old + snd inc)
    in 
        foldl' f (0, 0) points


lineOfInt :: IO [Int]
lineOfInt = return . map read . words =<< getLine

main :: IO ()
main = do  
    [n] <- lineOfInt
    xss <- sequence $ take n $ repeat lineOfInt
    (a, b) <- return $ score xss
    putStrLn $ show a ++ " " ++ show b

