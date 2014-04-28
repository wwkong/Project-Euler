-- Find the largest palindrome made from the product of two 3-digit numbers.

-- Print and write out the answer
main = do
        let ans = maximum [x*y | x <- [1.. 999], y <- [1.. 999], reverse (show(x*y)) == show (x*y)]
        writeFile "pe4.txt" $ show ans
        print ans
