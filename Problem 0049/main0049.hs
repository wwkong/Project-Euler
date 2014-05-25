{-
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual
in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are
permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property,
but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
-}

import           Data.List
import           Data.Numbers.Primes
import qualified Data.Set            as S

-- Create a function to get all prime permutations of a number in ascending order
pPerms :: Integer -> [Integer]
pPerms n =  let nStr = show n in
            sort $ filter isPrime $ map (read :: String -> Integer) $ permutations nStr

-- Define a function that choose 3 distinct elements from a lst
threeDistinct :: [Integer] -> [[Integer]]
threeDistinct lst = (S.toList . S.fromList) 
    [sort [a,b,c] | a <-lst, b <- lst \\ [a], c <- (lst \\ [a]) \\ [b]]

-- Define a function to find a three element arithmetic subsequence in a list if it exists
isArith :: [Integer] -> Bool
subArith :: [Integer] -> [Integer]
isArith lst = ((lst !! 1) - (lst !! 0)) == ((lst !! 2) - (lst !! 1))
subArith lst
    | elemLst == [] = []
    | otherwise = head elemLst
    where
        threeLst = threeDistinct lst
        elemLst = filter isArith threeLst

-- Find all such triples by filtering throughly
tLst :: [[Integer]]
tLst =  filter (\lst -> isPrime (lst !! 0) && (lst !! 0) /= (lst !! 1)) $
        (S.toList . S.fromList) $
        filter (/=[])
        [(subArith . pPerms) a | a <- takeWhile (<= 9997) $ dropWhile (<1000) primes]


-- Print and write out the answer
main :: IO()
main = do
        let ans = tLst
        writeFile "pe49.txt" $ show ans
        print ans
