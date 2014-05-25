-- Find the sum of all numbers which are equal to the sum of the factorial
-- of their digits.

-- We first note that the upper bound for this problem is 999999 (six nines)

import           Data.Char

-- Define our limit
limit :: Integer
limit = 999999

-- Memoization here is key; we first define an efficient factorial function
factrl :: Int -> Integer
factrl = (map factrl' [0..] !!) where
    factrl' 0 = 1
    factrl' n = (toInteger n) * factrl (n-1)

-- Define a function to check if a number is 'curious' ;D
isCurious :: Integer -> Bool
isCurious n = (== n) $ sum $ map factrl $ map digitToInt $ show n

-- Print and write out the answer
main :: IO()
main = do
        let ans = sum $ filter isCurious [3..limit]
        writeFile "pe34.txt" $ show ans
        print ans
