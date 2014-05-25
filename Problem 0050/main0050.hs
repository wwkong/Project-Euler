{-
Which prime, below one-million, can be written as the sum of the most consecutive primes?
-}

import           Data.Numbers.Primes

-- Trial and error suggests a good lower bound is 120 and so we take all primes under 100000
cPrimes :: [Int]
cPLens :: Int
cPrimes = takeWhile (<10000) primes
cPLens = length cPrimes

-- Define a function to find the max of our consecutive prime sum from (cPrimes) for a parameter n
-- that defines the number of consecutive entries
getMaxConsec :: Int -> Int
getMaxConsec n = getMaxConsec' 0 cPrimes where
    getMaxConsec' maxP lst
        | sum consecLst > 10^6 = maxP
        | len == n = max maxP consecP
        | otherwise = getMaxConsec' (max maxP consecP) (tail lst)
        where
            len = length lst
            consecLst = take n lst
            consecP = (if isPrime (sum consecLst) then sum consecLst else 0)

-- Import the tuple max function from OLD PE 14
tupMax :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupMax a b
    | max (fst a) (fst b) == fst a = a
    | max (fst a) (fst b) == fst b = b

-- Print and write out the answer
main :: IO()
main = do
        let ans = foldl1 tupMax [ (n, getMaxConsec n) | n <- [120.. cPLens] , getMaxConsec n /= 0]
        writeFile "pe50.txt" $ show ans
        print ans
