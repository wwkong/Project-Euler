-- Find the sum of the only eleven primes that are both truncatable from
-- left to right and right to left.

-- From the question, we are already given that 3797 is a truncatable prime.

import           Control.Monad
import           Data.Char
import           Data.Numbers.Primes

-- Create a function to determine if a number is left or right prime trucatable
lrpt :: Integer -> Bool
lrpt n = if lrptRtL lst then lrptLtR lst
         else False where
    lrptRtL nLst
        | nLst == [] = True
        -- | trace ("RtL Called with n=" ++ show nNum) False = undefined
        | not (isPrime nNum) = False
        | otherwise = lrptRtL $ init nLst
        where
            nNum = read $ map intToDigit nLst :: Integer
    lrptLtR nLst
        | nLst == [] = True
        -- | trace ("LtR Called with n=" ++ show nNum) False = undefined
        | not (isPrime nNum) = False
        | otherwise = lrptLtR $ tail nLst
        where
            nNum = read $ map intToDigit nLst :: Integer
    lst = map digitToInt $ show n

-- Generate a list of possible inputs for our function above
lrptLst :: [[Int]]
lrptNums :: [Integer]
lrptLst = foldr1 (++) [replicateM n [1,2,3,5,7,9] | n <- [2..]] -- first in list form
lrptNums = map (\lst -> read $ map intToDigit lst :: Integer) lrptLst -- then as a list of numbers

-- Print and write out the answer
main :: IO()
main = do
        let ans = sum $ take 11 $ filter lrpt lrptNums
        writeFile "pe37.txt" $ show ans
        print ans
