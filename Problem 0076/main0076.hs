-- How many different ways can one hundred be written as a sum of at least two positive integers?

-- Adapted from: 
-- http://www.haskell.org/haskellwiki/Euler_problems/71_to_80

-- Use a partition method to find the sum combinations
build :: [[Integer]] -> [[Integer]]
build x = (map sum (zipWith drop [0..] x) ++ [1]) : x

-- The head of the n-th element contains the following in order:
-- { Number of ways to create n-1 in groups,
--   Number of ways to create n-2 in groups,
--   ...,
--   Number of ways to create n-(floor n/2) in groups }

-- This is memoized using the information built in previous sublists

-- Note that this also includes 1 element partitions (using ++ [1]) so we subtract 
-- 1 from the sum of the head of the 100th iterate

-- Print and write out the answer
main :: IO()
main = do
        let ans = (sum $ head $ iterate build [] !! 100) - 1 :: Integer
        writeFile "pe76.txt" $ show ans
        print ans
