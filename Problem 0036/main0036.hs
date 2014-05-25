-- Find the sum of all numbers, less than one million, which are palindromic
-- in base 10 and base 2.

import           Data.Char
import           Numeric

-- Define a function to check if a number is a palindrome in base 10 and 2
isPalin :: Integer -> Bool
isPalin n = reverse nDec == nDec &&
            reverse nBin == nBin
        where
            nDec = map digitToInt $ show n
            nBin = showIntAtBase 2 intToDigit n ""

-- Print and write out the answer
main :: IO()
main = do
        let ans = sum [xs | xs <- [1.. (10^6-1)], isPalin xs == True]
        writeFile "pe36.txt" $ show ans
        print ans
