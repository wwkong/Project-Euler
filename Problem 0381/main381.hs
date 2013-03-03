import Data.List
import Data.Numbers.Primes
-- Implement an inverse generating function using the EEA
inverse n 1 = 1
inverse n p = (m * n + 1) `div` p
        where m = p - inverse p (n `mod` p)
-- We now apply a shortcut using Wilson's Theorem ( (p-1)! == -1 mod p iff p is prime )
-- It can be shown that S(p) = (p-5)!*9 mod p = -3/8 mod p
main = print (sum [(-3) * (inverse xs 8) `mod` xs | xs <- takeWhile (<10^2) (filter (>=5) primes)])
-- The variable 'prime' is taken from the Data.Numbers.Primes library and uses a lazy
-- sieve generator
-- Note that this takes a REALLY long time to complete     