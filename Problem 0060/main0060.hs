{-
The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes
and concatenating them in any order the result will always be prime. For example,
taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792,
represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate
to produce another prime.
-}

-- This is a modfied version of the code on p.1 of the PE60 forum

import Data.Numbers.Primes

-- Take some list of primes
pLst :: [Int]
pLst = takeWhile (<10000) primes

-- Given an initial prime x and a list, return all elements of the list which
-- can be concatenated with x to produce another prime 
concatPrime :: Int -> [Int] -> [Int]
concatPrime x lst = filter (\y -> isPrime (read (shows x (show y)) :: Int)
                               && isPrime (read (shows y (show x)) :: Int)) lst

-- Generate the list of candidate quintuples
solve :: [[Int]]
solve = do
     a <- pLst
     let m = concatPrime a $ dropWhile (<= a) pLst
     b <- m
     let n = concatPrime b $ dropWhile (<= b) m
     c <- n
     let o = concatPrime c $ dropWhile (<= c) n
     d <- o
     let p = concatPrime d $ dropWhile (<= d) o
     e <- p
     return [a,b,c,d,e]
  

-- Print and write out the answer
main :: IO()
main = do -- We guess that it is the first entry
        let ans = (sum . head) solve
        writeFile "pe60.txt" $ show ans
        print ans