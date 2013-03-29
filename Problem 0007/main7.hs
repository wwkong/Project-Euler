-- Took 0.02s to compute.

-- We use the built in prime number generator as a hack ;D
import Data.Numbers.Primes

-- Print and write out the answer
main = do 
		let ans = primes !! (10001 - 1) -- Done to handle Haskell indexing
		writeFile "pe7.txt" $ show ans
		print ans