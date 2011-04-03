module Main 
  (
    main
  ) where

import qualified Data.List as List
import Data.List (foldl', sort)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Array as Array
--import Data.Array (Array, (!))
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (liftM)
import Control.Exception (assert)
import qualified Data.Array as Array


magic :: Int -> Maybe [Int]
magic 2 = Nothing
magic p = 
    let sp   = spec p
        orig = [sp ! k | k <- [1..p-1]]
        pats = map (pattern p sp) [1..p-1]
        good = all (\pat -> pat == orig || pat == complement orig) pats
    in 
        if good then
            Just orig
        else
            Nothing

pow p n k = pow' p n k 1
pow' p n 0 acc = acc
pow' p n k acc = pow' p n (k - 1) (mod (acc * n) p)

complement :: [Int] -> [Int]
complement = map (\x -> if (x == 0) then 1 else 0)

primitive :: Int -> Int
primitive p =
    let isPrimitive a = not . any (== 1) $ [pow p a k | k <- [1 .. p-2]]
    in  head . filter isPrimitive $ [1 .. p-1]

pattern :: Int -> Map Int Int -> Int -> [Int]
pattern p m d = [m ! ((k * d) `mod` p) | k <- [1..p-1]]

spec :: Int -> Map Int Int
spec p = 
    let a = primitive p
    in Map.fromList [(pow p a k, k `mod` 2) | k <- [0..p-2]]

lineOfInt :: IO [Int]
lineOfInt = return . map read . words =<< getLine

main :: IO ()
main = do 
    [n] <- lineOfInt
    case n of 
        0 -> return ()
        n -> case magic n of
            Nothing -> (putStrLn "Impossible") >> main
            Just xs -> (putStrLn $ foldl (++) "" $ map show $ xs) >> main

