-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
-- how many letters would be used?

import Data.Maybe
import Data.Char

-- Create a dictionary for numbers
nums = [(0,""),
 (1,"one"),
 (2,"two"),
 (3,"three"),
 (4,"four"),
 (5,"five"),
 (6,"six"),
 (7,"seven"),
 (8,"eight"),
 (9,"nine"),
 (10,"ten"),
 (11,"eleven"),
 (12,"twelve"),
 (13,"thirteen"),
 (14,"fourteen"),
 (15,"fifteen"),
 (16,"sixteen"),
 (17,"seventeen"),
 (18,"eighteen"),
 (19,"nineteen"),
 (20,"twenty"),
 (30,"thirty"),
 (40,"forty"),
 (50,"fifty"),
 (60,"sixty"),
 (70,"seventy"),
 (80,"eighty"),
 (90,"ninety"),
 (100,"hundred"),
 (1000,"thousand")] 

-- Create a function to parse a number's length if spelled out in the British English system
numParser n 
        | n <= 20 = length $ fromJust $ lookup n nums  
        | n <= 99 = numParser(n `mod` 10) + 
        (length $ fromJust $ lookup ( n - (n `mod` 10)) nums)  
        | n <= 999 && n `mod` 100 /= 0 = 
        numParser(n `mod` 100) + (length "hundred") + 3 + -- for the "and" `
        (length $ fromJust $ lookup (quot n 100) nums)
        | n <= 999 = 
        numParser(n `mod` 100) + (length "hundred") +
        (length $ fromJust $ lookup (quot n 100) nums)
        | n == 1000 = (length $ fromJust $ lookup n nums) +
        (length $ fromJust $ lookup (quot n 1000) nums)

-- Print and write out the answer		
main = do
		let ans = sum $ [numParser ns | ns <- [1.. 1000]]
		writeFile "pe17.txt" $ show ans
		print ans