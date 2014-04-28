{-
An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value of the following expression.

d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000
-}

import           Data.Char
import           Data.List

-- Create a function to find the n-th digit of Champernowne's constant
champ :: Int -> Int
champ n = digitToInt $ champStr !! n where
    champStr = concatMap show [n | n <- [0..]]

-- Print and write out the answer
main = do
        let ans =
                champ 1 * champ 10 * champ (10^2) *
                champ (10^3) * champ (10^4) *
                champ (10^5) * champ (10^6)
        writeFile "pe40.txt" $ show ans
        print ans
