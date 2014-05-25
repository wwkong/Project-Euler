{-
A number chain is created by continuously adding the square of the digits in a number
to form a new number until it has been seen before.

For example,

44 → 32 → 13 → 10 → 1 → 1
85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. 
What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

How many starting numbers below ten million will arrive at 89?
-}

-- Based on the official Haskell PE92 solution and the method used in the PE92 forums
-- by cyph1e

import           Data.Array
import           Data.Char
import           Data.List

-- Create the set of all increasing sequences for a set number of digits
-- Note: The first number is always 00...0
baseNums :: Int -> Int -> [[Int]]
baseNums 1      minDigit  = [[a]|a<-[minDigit..9]]
baseNums digits minDigit  = [a:b|a<-[minDigit..9], b <- baseNums (digits-1) a]

--  Create an efficient method to calculate the sum of the squares of the digits of a number
squares :: Array Char Int
squares = array ('0','9') [ (intToDigit x, x^2) | x <- [0..9] ]
sumSquares :: Int -> Int
sumSquares = sum . map (squares !) . show

-- Analogously, import our functions from PE74 and create a factorial function
facts :: Array Int Int
factorial :: Int -> Int
facts = array (0,9) [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120),(6,720),(7,5040),(8,40320),(9,362880)]
factorial n = facts ! n

-- Calculate the multinomial coefficient for a list of digits
multCoeff:: [Int] -> Int
multCoeff xs = factorial 7 `div` (product $ map (factorial . length) $ group xs)

-- For a starting number in the form of a list of digits, find the terminal value of the chain
lastNum :: [Int] -> Int
lastNum lst = until (\x -> x == 89 || x == 1) sumSquares n
	where
		n = sum $ map (^2) lst

-- Print and write out the answer
main :: IO()
main = do
		let nums = [multCoeff a | a <- tail $ baseNums 7 0, lastNum a == 89]
		let ans = sum nums
		writeFile "pe92.txt" $ show ans
		print ans
