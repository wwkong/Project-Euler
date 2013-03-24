import Data.Char
<<<<<<< HEAD
main = print (sum $ map digitToInt $ show (2^1000))
=======

-- Print and write the 
main = do
		let ans = sum $ map digitToInt $ show (2 ^ 1000)
		writeFile "pe16.txt" $ show ans
		print ans
>>>>>>> Modified existing Haskell solutions
