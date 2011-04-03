{-# LANGUAGE BangPatterns #-}

module Main 
  (
    main
  ) where

import qualified Data.List as List
import Data.List (foldl', unfoldr, sort)
import qualified Data.Array as Array
import Data.Array (Array, (!))

import Control.Monad (liftM)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (assert)


magic :: Int -> Maybe [Int]
magic 2 = Nothing
magic p = 
    let m    = spec p
        good = and [matches p m d | d <- [2..p-1]]
    in 
        if good then Just [m ! k | k <- [1..p-1]] else Nothing

matches :: Int -> Array Int Int -> Int -> Bool
matches p m d = 
    (and [m ! k == m ! ((k * d) `mod` p) | k <- [1..p-1]]) || 
    (and [m ! k /= m ! ((k * d) `mod` p) | k <- [1..p-1]])

-- gives list of a^0, a^1, ..., a^n-1, mod p
powers :: Int -> Int -> Int -> [Int]
powers p a n = unfoldr f (1, n)
    where f (_, 0)   = Nothing
          f (acc, m) = Just (acc, ((acc * a) `mod` p, m-1))

-- finds a generator for the multiplicitive group Z_p*
-- which always exists for prime p
primitive :: Int -> Int
primitive p = head . filter isPrimitive $ [1 .. p-1]
    where isPrimitive a = not . any (== 1) . tail $ powers p a (p-1)

spec :: Int -> Array Int Int
spec p = Array.array (1, p - 1) $ zip ak parities
    where a = primitive p
          ak = powers p a (p-1)
          parities = cycle [0,1]

lineOfInt :: IO [Int]
lineOfInt = return . map read . words =<< getLine

main :: IO ()
main = do 
    [n] <- lineOfInt
    case n of 
        0 -> return ()
        n -> case magic n of
            Nothing -> (putStrLn "Impossible") >> main
            Just xs -> (putStrLn . concat . map show $ xs) >> main

