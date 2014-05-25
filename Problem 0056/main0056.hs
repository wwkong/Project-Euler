{-
Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?
-}

import           Data.Char

-- Define a function to compute the digital sum of a number
digitalSum :: Integer -> Int
digitalSum n = sum $ map digitToInt $ show n

-- We apply a straightforward pattern matched list (brute force) to get all possible digital sums
dSums :: [Int]
dSums = map digitalSum [a ^ b | a <- ([1.. 100] :: [Integer]), 
								b <- ([1.. 100] :: [Integer])]

-- Print and write out the answer
main :: IO()
main = do
        let ans = maximum dSums
        writeFile "pe56.txt" $ show ans
        print ans
