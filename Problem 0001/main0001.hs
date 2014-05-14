-- Find the sum of all the multiples of 3 or 5 below 1000.

-- Print and write out the output
main :: IO()
main = do
        let ans = (sum [x | x <- [1..999] :: [Integer], x `mod` 3 == 0 || x `mod` 5 == 0])
        writeFile "pe1.txt" $ show ans
        print ans
        