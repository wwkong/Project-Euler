{-
How many, not necessarily distinct, values of  nCr, for 1 <= n <= 100, are greater than one-million?
We are given that n = 23, r = 10 is one such value
-}

-- Implement an efficient choose function (credit goes to the Stack Exchange)
choose :: Int -> Int -> Int
choose _ 0 = 1
choose 0 _ = 0
choose n k = choose (n-1) (k-1) * n `div` k

-- Implement a function that returns a set of choose function values for fixed n
chooseN :: Int -> [Int]
chooseN n = [ choose n k | k <- [1.. n] ]

-- Apply the above function to 23 <= n <= 100 and return a list of all values
allCombinations :: [Int]
allCombinations = foldl1 (++) $ map chooseN [23.. 100]

-- Print and write out the answer
main :: IO()
main = do
        let ans = length $ filter (>10^6) allCombinations
        writeFile "pe53.txt" $ show ans
        print ans
