-- Find the value of n, 1<n<10^7, for which φ(n) is a permutation of 
-- n and the ratio n/φ(n) produces a minimum.

-- Using Problem #69 as a guide, we see that the minimum will most
-- be a product of powers of a small number of primes so we 
-- will just iterate through the powers of the first couple primes
-- until we find a number that is a permutation

import Data.Numbers.Primes
import Data.List

-- Our first guess shall be the set of semi-primes around sqrt(10^7) 
-- with about a 1000 unit tolerance
intSqrt = floor . sqrt . (fromInteger :: Integer -> Float)
range = filter isPrime [intSqrt(10^7)-1000.. intSqrt(10^7)+1000]

-- Note that for primes p,q, phi (p*q) = (1-p) * (1-q)
phi [p,q] = (1-p) * (1-q)

-- We now build up our candidate list, phi lst and prod lst
cLst = (filter (\[a,b] -> a /= b) . sequence) [range,range]
phiLst = map phi cLst
prodLst = map product cLst

-- Create a permutation comparison function and a specialized minimumBy function
perm a b = (sort . show) a == (sort . show) b
minPhiN = minimumBy (\(a,b) (c,d) -> compare 	((fromInteger b) / (fromInteger a))
								((fromInteger d) / (fromInteger c)))

-- Print and write out the answer
main = do
		let zipLst = zip phiLst prodLst
		let ans = (snd . minPhiN) $ filter (\(a,b) -> b <(10^7)) $ filter (\(a,b) -> perm a b) zipLst
		writeFile "pe70.txt" $ show ans
		print ans
