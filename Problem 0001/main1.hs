-- Print and write out the output
-- Took 0.00s to compute.
main = do 
		let ans = (sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0])
		writeFile "pe1.txt" $ show ans
		print ans