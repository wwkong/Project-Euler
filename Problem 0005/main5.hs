<<<<<<< HEAD
import Data.List
main = print (foldr1 lcm [1..20])
=======
-- Print and write out the answer
main = do 
		let ans = foldr1 lcm [1..20]
		writeFile "pe5.txt" $ show ans
		print ans
>>>>>>> Modified existing Haskell solutions
