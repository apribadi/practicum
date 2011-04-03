module Main 
  (
    main
  ) where

import qualified Data.List as List
import Data.List (foldl', sort)
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Array (Array, (!))
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (liftM)
import Control.Exception (assert)
import qualified Data.Array as Array


magic :: Int -> Maybe [Int]
magic 2 = Nothing
magic p = 
    let m    = spec p
        good = and [matches p m d | d <- [2..p-1]]
    in 
        if good then Just [m!k|k<-[1..p-1]] else Nothing

matches :: Int -> Array Int Int -> Int -> Bool
matches p m d = 
    (and [m ! k == m ! ((k * d) `mod` p) | k <- [1..p-1]])
    || (and [m ! k /= m ! ((k * d) `mod` p) | k <- [1..p-1]])

series p a n = series' p a n 1
series' p a 0 acc = []
series' p a n acc = acc : series' p a (n-1) q
    where q = (acc * a) `mod` p


primitive :: Int -> Int
primitive p =
    let isPrimitive a = not . any (== 1) . tail $ series p a (p-1)
    in  head . filter isPrimitive $ [1 .. p-1]

spec :: Int -> Array Int Int
spec p = 
    let a = primitive p
    in Array.array (1, p-1) $ zip (series p a (p-1)) [k `mod` 2 | k <- [0..p-2]]

lineOfInt :: IO [Int]
lineOfInt = return . map read . words =<< getLine

main :: IO ()
main = do 
    [n] <- lineOfInt
    case n of 
        0 -> return ()
        n -> case magic n of
            Nothing -> (putStrLn "Impossible") >> main
            Just xs -> (putStrLn . foldl (++) "" . map show $ xs) >> main

