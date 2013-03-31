-- It can be shown that for a numerical value of given word S, that
-- 1+8*S is a perfect square and sqrt(1+8*S) is odd, through the 
-- quadratic equation

-- Took 0.03s to compute.

import Data.Char
import Data.List
import Data.List.Split

-- Import the naive square root function
squareRoot :: Integer -> Integer
squareRoot = floor . sqrt . (fromIntegral :: Integer -> Double)

-- Define a function that converts a letter to its alphabetic position
fromLetter :: Char -> Integer
fromLetter c = toInteger $ (ord c) - 64

-- Define a function to check if a word is a triangle word
isTWord :: String -> Bool
isTWord s 
	| discrim ^ 2 == 1+8*sNum && odd discrim = True
	| otherwise = False
	where 
		sNum = foldl1 (+) $ map fromLetter s
		discrim = squareRoot (1+8*sNum)
	
-- Print and write out the answer
main = do
		rawWordData <- readFile "words.txt"
		let wordData = splitOn "," $ filter (/='"') rawWordData
		let ans = length [wds | wds <- wordData, isTWord wds]
		writeFile "pe42.txt" $ show ans
		print ans