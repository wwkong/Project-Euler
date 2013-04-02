-- Print and write out the answer
main = do 
		let ans = head [a*b*c| a <- [1..500], b <- [1.. 500], let c = 1000 - a - b , a^2 + b^2 == c^2]
		writeFile "pe9.txt" $ show ans
		print ans