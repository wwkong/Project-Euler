-- How many different ways can one hundred be written as a sum of at least two positive integers?

-- Adapted from: 
-- http://www.haskell.org/haskellwiki/Euler_problems/71_to_80

-- Use a partition method to find the sum combinations
build :: [[Integer]] -> [[Integer]]
build x = (map sum (zipWith drop [0..] x) ++ [1]) : x

-- Print and write out the answer
main :: IO()
main = do
        let ans = (sum $ head $ iterate build [] !! 100) - 1 :: Integer
        writeFile "pe76.txt" $ show ans
        print ans
