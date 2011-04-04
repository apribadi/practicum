module Main 
  (
    main
  ) where

import Data.List (foldl', unfoldr, sort)

{-
import qualified Data.Array as Array
import Data.Array (Array, (!))
import Control.Monad (liftM)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (assert)
-}


-- Gives list of a^0, a^1, ..., a^n-1, mod p.
powers :: Int -> Int -> Int -> [Int]
powers p a n = unfoldr f (1, n)
    where f (_, 0)   = Nothing
          f (acc, m) = Just (acc, ((acc * a) `mod` p, m-1))

-- Finds a generator for the multiplicitive group Z_p*
-- which always exists, given prime p.
primitive :: Int -> Int
primitive p = head $ filter isPrimitive [1 .. p-1]
    where isPrimitive a = not . any (== 1) . tail $ powers p a (p - 1)

-- Is very magical.
magic :: Int -> Maybe [Int]
magic 2 = Nothing
magic p = Just . map snd . sort $ zip (powers p a (p - 1)) (cycle [0, 1])
    where a = primitive p

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

