import Data.List
import Data.Char

<<<<<<< HEAD
-- Full credit goes to OJ's rants

main = do
		pe8str <- readFile "data.txt"
		let pe8num = map digitToInt pe8str
		let chunks = c' pe8num 5 995
				where
				c' _ _ 0 = []
				c' l n c = (take n l) : c' (tail l) n (c - 1)
		print $ foldr1 max
			  $ map product chunks
			  
=======
-- Define a function that computes an array to calculate the product of 
-- 5 consecutive numbers of elements in an array
mConsecFive :: [Int] -> [Int]
mConsecFive ns = mConsecFive' ns ns 5 where
	mConsecFive' :: [Int] -> [Int] -> Int -> [Int]
	mConsecFive' ns nss c
		| c == 1 = ns
		| otherwise = mConsecFive' ms (tail nss) (c-1) 
			where ms = zipWith (*) (init ns) (tail nss)

-- Print and write out the answer
main = do 
		rawContents <- readFile "data.txt"
		let mContents = map digitToInt rawContents
		let ans = foldr1 max $ mConsecFive mContents
		writeFile "pe8.txt" $ show ans
		print ans
>>>>>>> Modified existing Haskell solutions
