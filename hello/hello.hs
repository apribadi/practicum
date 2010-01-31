module Main where

import Data.List
import Control.Monad

-- Digits, least significant first
digits :: Int -> Int -> [Int]
digits base = unfoldr f where
    f 0 = Nothing
    f n = Just (n `mod` base, n `div` base)

-- Whether d divides n evenly
divides :: Int -> Int -> Bool
divides d n = (n `mod` d) == 0

-- Whether is prime
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = not $ any (\d -> divides d n) [2..n-1]

-- Whether there are any digits of 1 in base 11
bEleven :: Int -> Bool
bEleven = any (== 1) . digits 11

-- Whether the sum of the digits in binary is divisible by 4
binary :: Int -> Bool
binary = divides 4 . sum . digits 2

-- What to do for a given number
action :: Int -> IO ()
action n | isPrime n && bEleven n && binary n = sequence_ shrinking
         | isPrime n && bEleven n             = sequence_ growing
         | isPrime n                          = sequence_ shrinking
         | otherwise                          = sequence_ growing
    where
        growing = map putStrLn $ [take n "I<3Cows" | n <- [1..7]]
        shrinking = reverse growing

-- I/O procedure
main :: IO ()
main = do
    [low, high] <- liftM (map read . words) $ getLine
    sequence_ $ map action [low..high]

