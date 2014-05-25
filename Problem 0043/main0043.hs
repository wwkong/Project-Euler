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

import           Data.Char
import           Data.List
import qualified Data.List.Stream    as S

-- Taken from:
-- http://stackoverflow.com/questions/18124303/haskell-create-unique-n-tuples-from-a-list-of-x-elements
choose :: Int -> String -> [String]
choose n list = concatMap permutations $ choose' list [] where
  choose' []     r = if length r == n then [r] else []
  choose' (x:xs) r | length r == n = [r]
                   | otherwise     = choose' xs (x:r) ++ choose' xs r

-- Create a function to iterative build up the combinations
nextPanNums :: Int -> [String] -> [String]
nextPanNums p nums = [0..9] >>= nextPanFn nums
    where   nextPanFn sLst n =
                S.map (\s -> show n ++ s) $
                S.filter    (\num -> 
                                (read (show n ++ take 2 num) :: Int) `mod` p == 0 &&
                                 not (intToDigit n `elem` num)) sLst

-- Iterate over the first 7 primes in reverse
panNums :: String -> [String]
panNums nums = getNums subPanNums
    where   getNums = ( nextPanNums 1 . nextPanNums 2  . nextPanNums 3  . nextPanNums 5 . 
                        nextPanNums 7 . nextPanNums 11 . nextPanNums 13 )
            subPanNums = filter (\x -> (read x :: Integer) `mod` 17 == 0) $ choose 3 nums

-- Print and write out the answer
main :: IO()
main = do
        let ans = sum $  map (read :: String -> Integer) $ panNums "0123456789"
        writeFile "pe46.txt" $ show ans
        print ans
