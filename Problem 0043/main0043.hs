{-
The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits
0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

d2d3d4=406 is divisible by 2
d3d4d5=063 is divisible by 3
d4d5d6=635 is divisible by 5
d5d6d7=357 is divisible by 7
d6d7d8=572 is divisible by 11
d7d8d9=728 is divisible by 13
d8d9d10=289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.
-}

import           Data.List

-- Get all pandigital numbers as strings
panNums = permutations "0123456789"

-- Check the divisibility condition for a panNum string
isPanDiv :: String -> Bool
isPanDiv nStr
    | (takeThree 2) `mod` 2 /= 0 = False
    | (takeThree 3) `mod` 3 /= 0 = False
    | (takeThree 4) `mod` 5 /= 0 = False
    | (takeThree 5) `mod` 7 /= 0 = False
    | (takeThree 6) `mod` 11 /= 0 = False
    | (takeThree 7) `mod` 13 /= 0 = False
    | (takeThree 8) `mod` 17 /= 0 = False
    | otherwise = True
    where takeThree n = (read :: String -> Integer) $ take 3 $ snd $ splitAt (n-1) nStr

-- Print and write out the answer
main = do
        let ans = sum [read pNums :: Integer | pNums <- panNums , isPanDiv pNums]
        writeFile "pe46.txt" $ show ans
        print ans
