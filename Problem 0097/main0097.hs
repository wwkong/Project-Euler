-- 28433*2^(7830457)+1.
-- Find the last ten digits of this prime number.

-- Create a mod 10^10 exp function for a^b
modExp :: Int -> Int -> Int
modExp a b = foldl1 (\a1 a2 -> a1*a2 `mod` 10^10) $ take b $ repeat a

-- Print and write out the answer
main :: IO()
main = do
        let ans = (28433 * (modExp 2 7830457) `mod` 10^10) + 1
        writeFile "pe97.txt" $ show ans
        print ans
