{-
The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3).
In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.
-}

import           Data.List
import qualified Data.Map  as Map

-- Here, we are relying on Haskell's number theory library to handle the precision and grunt work
-- We also use the Data.Map module to create updateable dictionaries

-- Define a function that converts a number into string, sorts it and returns back the number as a string
ordNum :: Integer -> String
ordNum n =  sort (show n)

-- Iterate over the natural numbers until we find a permutation that has n instances and returns the
-- the base k cubed
iterCubes :: Integer -> Integer
iterCubes m = iterCubes' m 1 Map.empty where
    iterCubes' n k mp
        | nCubePerm /= Map.fromList [] = k^3
        | otherwise = iterCubes' n (k+1) cubeMap
        where
            nCubePerm = Map.filter (==n) cubeMap
            cubeMap = Map.insertWith (+) (ordNum (k^3)) 1 mp

-- Given a number n, define a function that returns the first number that is a permutation of
-- n when cubed
firstPermCube :: Integer -> Integer
firstPermCube n = head [k | k <- [1..n], ordNum (k^3) == ordNum n]

-- Print and show the answer
main :: IO()
main = do
        let ans = ((firstPermCube . iterCubes) 5) ^ 3
        writeFile "pe62.txt" $ show ans
        print ans
