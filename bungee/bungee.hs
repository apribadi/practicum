module Main (main) where

data Outcome = Stuck | Killed | Survives

bungee k l s w
    | energy   k l s w < 0  = Stuck
    | velocity k l s w > 10 = Killed
    | True                  = Survives

energy   k l s w = 9.81 * w * s - (1/2) * k * (s - l)^2
velocity k l s w = sqrt (2 * (energy k l s w) / w)

readLine :: (Read a) => IO [a]
readLine = return . map read . words =<< getLine

main :: IO ()
main = do 
    line <- readLine
    case line of 
        [0, 0, 0, 0] -> return ()
        [k, l, s, w] -> case bungee k l s w of
            Stuck    -> putStrLn "Stuck in the air."
            Killed   -> putStrLn "Killed by the impact."
            Survives -> putStrLn "James Bond survives."
            >> main
            
