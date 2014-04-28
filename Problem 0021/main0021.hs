-- Evaluate the sum of all the amicable numbers under 10000.

-- Make sure to compile using the -O flag! Single threaded processing makes this program REALLY slow

-- Create a proper divisors generator and sum function
sumDivs :: Int -> Int
sumDivs n = sum [xs| xs <- [1..  div n 2], n `mod` xs == 0]

-- Print and write out the answer
main = do
        let ans = sum [xs | xs <- [1..9999], xs == (sumDivs . sumDivs) xs, sumDivs xs /= xs]
        writeFile "pe21.txt" $ show ans
        print ans
