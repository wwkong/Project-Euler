-- This is more of a combinatorics problem than a programming one (20x20 grid has 40 C 20 combinations)

-- Create a nCm function 
choose n 0 = 1
<<<<<<< HEAD
choose n k = ((choose (n-1) (k-1)) * n) `div` k 
main = print (choose 40 20)
=======
choose n k = choose (n-1) (k-1) * n `div` k 

-- Print and write out the answer
main = do 
		let ans = choose 40 20
		writeFile "pe15.txt" $ show ans
		print ans
>>>>>>> Modified existing Haskell solutions
