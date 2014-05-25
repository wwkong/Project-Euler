{-
Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
where each “_” is a single digit.
-}

import 			 Data.Char

-- Based off of the approach described in the PE206 forum by pddarkdave
-- Note: That man is a genius!

-- Define our upper and lower bounds
minB :: Integer
maxB :: Integer
minB = floor $ sqrt $ fromIntegral 1020304050607080900 :: Integer
maxB = floor $ sqrt $ fromIntegral 1929394959697989990 :: Integer

-- For an input list of integers, show the integer representation concatenated
readLst :: [Int] -> Integer
readLst lst = read $ concatMap show lst :: Integer

-- Get the significant and insignificant digits of a number list
sigDigits :: [Int] -> [Int]
insigDigits :: [Int] -> [Int]
sigDigits nLst = [nLst !! k | k <- [0,2..18]]
insigDigits nLst = [nLst !! k | k <- [1,3..17]]

-- For a list of integers, find the next lexigraphical number
nextLex :: [Int] -> [Int]
nextLex lst = padding ++ mainLst
	where
		padding = replicate (length lst - length mainLst) 0
		mainLst	= map digitToInt $ show nextNum
		nextNum = (read $ concatMap show lst :: Integer) + 1

-- For a given candidate list (of the square), find the next candidate number
nextCand :: [Int] -> Integer
nextCand lst
	| otherwise = nextNum
	where
		nextNum = 10 * (read $ concatMap show $ concat nextNumLst)
		nextNumLst = zipWith (\a b -> [a,b]) [1..9] newInsigs
		sigs = sigDigits lst
		postSigs = drop maskLen sigs
		maskLen = length . filter (==True) $ zipWith (==) sigs [1..9]
		preInsigs = if head postSigs < (maskLen + 1) then
						take maskLen (insigDigits lst)
					else nextLex $ take maskLen (insigDigits lst)
		postInsigs = replicate (length lst - maskLen) 0
		newInsigs = preInsigs ++ postInsigs

-- Use the above to produce the main iterating function
iterFind :: Integer -> Integer
iterFind n
	| n > maxB       		            = 0
	| sigDigits sqrLst == [1..9] ++ [0] = n
	| otherwise = iterFind (max next (n+10))
	where
		sqrLst = map digitToInt (show (n^2))
		next = 10 * (ceiling $ sqrt (fromIntegral $ nextCand sqrLst) / 10)

-- Print and write out the ans
main :: IO()
main = do
		let ans = iterFind minB 
		writeFile "pe206.txt" $ show ans
		print ans
