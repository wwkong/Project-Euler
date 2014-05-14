-- Find the sum of all the primes below two million.

-- More prime number hacks! ;D
import           Data.Numbers.Primes

-- Print and write out the answer
main :: IO()
main = do
        let ans = sum $ takeWhile (< 2000000) primes :: Integer
        writeFile "pe10.txt" $ show ans
        print ans
