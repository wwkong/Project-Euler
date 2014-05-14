-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- Print and write out the answer
main :: IO()
main = do
        let ans = head [a*b*c | a <- [1..500] :: [Integer], b <- [1.. 500] :: [Integer], 
                        let c = 1000 - a - b, a^2 + b^2 == c^2]
        writeFile "pe9.txt" $ show ans
        print ans
