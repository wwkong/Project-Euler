-- More prime number hacks! ;D
import Data.Numbers.Primes

-- Print and write out the answer
main = do
		let ans = sum $ takeWhile (< 2000000) primes
		writeFile "pe10.txt" $ show ans
		print ans