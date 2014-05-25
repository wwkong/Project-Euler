{-
It is possible to show that the square root of two can be expressed as an infinite continued fraction.

 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

By expanding this for the first four iterations, we get:

1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first
example where the number of digits in the numerator exceeds the number of digits in the denominator.

In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?
-}

import           Data.Ratio

-- Here, we are going to take advantage of the base Data.Ratio library and its arbitrary precision point
-- rational data type

-- Define a function to find the n-th fractional expansion of sqrt(2)
sqrtTwoExp :: Int -> Ratio Int
sqrtTwoExp n = 1 + (sqrtTwoExp' (n-1)) where
    sqrtTwoExp' 0 = 1 % 2
    sqrtTwoExp' k = 1 / (2 + (sqrtTwoExp' (k-1)))

-- Define a function to check if a rational's numerator has more digits than its denominator
bigNumerator :: Ratio Int -> Bool
bigNumerator r = (length . show . numerator) r > (length . show . denominator) r

-- Print and write out the answer
main :: IO()
main = do
        let ans = length [ns | ns <- [1.. 1000], (bigNumerator . sqrtTwoExp) ns]
        writeFile "pe57.txt" $ show ans
        print ans
