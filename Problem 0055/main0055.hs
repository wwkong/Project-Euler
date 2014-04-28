-- How many Lychrel numbers are there below ten-thousand?
-- We are given that 349, 47 are not Lychrel and 196, 10677 are

-- Create a function to check if a number is Lychrel
isLychrel n = isLychrel' n 0 where
    isLychrel' n iter
        | iter == 50 = True
        | nStr == reverse nStr && iter > 0 = False
        | otherwise = isLychrel' (n + nRev) (iter + 1)
        where
            nStr = show n
            nRev = read (reverse nStr) :: Integer

-- Print and write out the answer
main = do
        let ans = length [n | n <- [1.. (10^4-1)], isLychrel n]
        writeFile "pe55.txt" $ show ans
        print ans
