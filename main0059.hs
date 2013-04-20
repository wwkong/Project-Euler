{-
Using cipher1.txt, a file containing the encrypted ASCII codes, and the knowledge that the plain text must 
contain common English words, decrypt the message and find the sum of the ASCII values in the original text.

The decription method used is an XOR cipher with a 3 letter lowercase key and the language used is English
-}

import Data.Bits
import Data.List
import Data.Char
import Data.Function
import Data.List.Split

{-
-- Start off with some frequecy analysis, assuming that `space` is the most frequent character

-- Create a fuction that takes a list of groups and sorts them based on length with the result
-- in the format (elem, length)
sortGroup :: [[Int]] -> [(Int,Int)]
sortGroup grp = reverse $ sortBy (compare `on` snd) [(head elem, length elem) | elem <- grp]

-- Import our data here first and get the n-th most frequent character for each set
getFreqData n = do
				let parseNums = (map (read :: String -> Int)) . (splitOn ",") . head . words
				encryptedMsg <- fmap parseNums $ readFile "cipher1.txt"
				let len = length encryptedMsg
				let set1 = (sortGroup . group . sort) [encryptedMsg !! n | n <- [0,0+3..len-1]]
				let set2 = (sortGroup . group . sort) [encryptedMsg !! n | n <- [1,1+3..len-1]]
				let set3 = (sortGroup . group . sort) [encryptedMsg !! n | n <- [2,2+3..len-1]]
				print [set1 !! (n-1),set2 !! (n-1),set3 !! (n-1)]
-}

-- Using the above code, we see that the most frequent numbers in our 3 groups, in order, (due to the key being 
-- 3 characters) are 71, 79, and 68. Since the ASCII for `space` is 32 then 32 `xor` 71, 32 `xor` 79, and
-- 32 `xor` 68 are the three ASCII numbers for our key. 

-- Running the below gives our key, which is "god"
-- main = print $ map chr [32 `xor` 71, 32 `xor` 79, 32 `xor` 68]

-- We now start decrpytion

-- Create a decrypt function for a 3 letter key
decrypt :: String -> String -> String
decrypt msg key = 
	let 
		parsedKey = ((map ord) . concat . repeat) key
		parsedMsg = map ord msg
	in map chr $ zipWith xor parsedKey parsedMsg
					
-- Print and write out the answer
main = do
		let parseMsg = (map chr) . (map (read :: String -> Int)) . (splitOn ",") . head . words
		encryptedMsg <- fmap parseMsg $ readFile "cipher1.txt"
		let msg = decrypt encryptedMsg "god"
		-- print msg -- Print the message here if you want
		let ans = sum $ map ord msg
		writeFile "pe59.txt" $ show ans
		print ans
