{-
Given

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49
-}

-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed
-- in the same way?

{-
It can be shown that given a side length n of the sqaure, the corners of the square will
have sum: 4*cn + (1+2+3)*(n-1) = 4*cn + 6*(n-1) where cn is the maximum valued corner of the
(n-1) sized sub-square plus (n-1).

To add, the smallest valued corner of the n sized square is the value of the
largest corner of the (n-1) sized sub-square which can be caclulated as
3*((n-2)-1) + ((lastSum - 6*((n-2)-1)) `div` 4) + (n-1).

The elements of the n sized square form an arithmetic sequence with difference (n-1).
-}

-- Define a function to calculate the sum of the cross diagonal
crossDiagSum :: Integer -> Integer
crossDiagSum n
    | even n = error "You entered an even side length!"
    | n == 1 = 1
    | n == 3 = 24
    | otherwise =  4*cn + 6*(n-1)
    where
        lastSum = crossDiagSum (n-2)
        cn = 3*((n-2)-1) + ((lastSum - 6*((n-2)-1)) `div` 4) + (n-1)

-- Print and write out the answer
main = do
        let ans = sum $ map crossDiagSum $ filter odd [1..1001]
        writeFile "pe28.txt" $ show ans
        print ans
