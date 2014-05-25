{-
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c},
there are exactly three solutions for p = 120.
For which value of p  1000, is the number of solutions maximised?
-}

import           Data.List
import           Data.Set

-- Define a function that will return the number of solutions for a given parameter
numSolns :: Int -> Int
numSolns p = size $ fromList [ sort [a,b,p-b-a] | a <- [1..p], b <- [1..p], a^2 + b^2 == (p-a-b) ^2 ]

-- Import our tuple max function from OLD PE 14
tupMax :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupMax a b
    | max (fst a) (fst b) == fst a = a
    | max (fst a) (fst b) == fst b = b

-- Print and write out the answer
main :: IO()
main = do
        let ans = foldr1 tupMax [ (numSolns n, n) | n <- [1..999] ]
        writeFile "pe39.txt" $ show ans
        print ans
