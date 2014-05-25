-- Find the sum of all the numbers that can be written as the sum
-- of fifth powers of their digits.

import           Data.Char

-- We first note that the upper limit is 999999 (six digts)
-- Define a function to return the sum of the fifth powered digits
fPwr :: Int -> Int
fPwr n = sum $ map (^5) $ map digitToInt $ show n

-- Define a function to count the number of summed digit fifth powers equal
-- to the original given a limit
sumFPwr :: Int -> Int
sumFPwr k = sumFPwr' 2 k 0 where -- Disregard 0 and 1
    sumFPwr' n lim sumTot
        | n > lim = sumTot
        | sumF == n = sumFPwr' (n+1) lim (sumTot + sumF)
        | otherwise = sumFPwr' (n+1) lim sumTot
        where
            sumF = fPwr n

-- Print and write out the result
main :: IO()
main = do
        let ans = sumFPwr 999999
        writeFile "pe30.txt" $ show ans
        print ans
