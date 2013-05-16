{-
The 11K text file, roman.txt, contains one thousand numbers written in valid, but not 
necessarily minimal, Roman numerals; that is, they are arranged in descending units 
and obey the subtractive pair rule.

Find the number of characters saved by writing each of these in their minimal form.
-}

import Data.List (sortBy, unfoldr)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

-- Create a dictionary of roman numerals
romDict = sortBy (\(a,b) (c,d) -> compare d b)
	[("I",1),("V",5),("X",10),("L",50),("C",100),("D",500),("M",1000),
	 ("IV",4),("IX",9),("XL",40),("XC",90),("CD",400),("CM",900)]

-- Create a reverse dictionary/list of roman numerals
revRomDict = map (\(a,b) -> (b,a)) romDict

-- Define a function to convert a roman numeral string to an integer
romToNum :: String -> Int
romToNum rms = romToNum' rms romDict where
	romToNum' r dict
		| null dict = 0
		| take 1 r == headChar = headNum + (romToNum' (tail r) dict)
		| take 2 r == headChar = headNum + (romToNum' ((tail .tail) r) (tail dict))
		| otherwise = romToNum' r (tail dict)
		where 
			headNum = (snd . head) dict
			headChar = (fst . head) dict

-- Create a function to take in an integer and produce the minimal roman 
-- numeral representation

-- First define the seed generator for unfoldr
romSeeder :: (Int,[(Int,String)]) -> Maybe (String, (Int,[(Int,String)]))
romSeeder (n,dict)
	| null dict = Nothing
	| n < headNum = romSeeder (n,tail dict)
	-- | trace ("headNum=" ++ show headNum ++ " n=" ++ show n ++ " dict=" ++ show dict) False = undefined
	| otherwise = Just (headStr, (n - headNum, dict))
	where 
		headNum = fst (head dict)
		headStr = snd (head dict)

-- Define the main converting function
numToRom :: Int -> String
numToRom n = concat $ unfoldr romSeeder (n,revRomDict)

-- Define a function to calculate the characters saved from transforming 
-- an inefficient roman numeral to an efficient one
savedChars rm = (length rm) - ((length . numToRom . romToNum) rm)

-- Print and write out the answer
main = do
		contents <- fmap lines $ readFile "roman.txt"
		let ans = sum $ map savedChars contents
		writeFile "pe89.txt" $ show ans
		print ans