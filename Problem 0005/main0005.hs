-- What is the smallest positive number that is evenly divisible
-- by all of the numbers from 1 to 20?

-- Print and write out the answer
main :: IO()
main = do
        let ans = foldr1 lcm ([1..20] :: [Integer])
        writeFile "pe5.txt" $ show ans
        print ans
