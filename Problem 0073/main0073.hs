{-
Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, 
it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 
4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 3 fractions between 1/3 and 1/2.

How many fractions lie between 1/3 and 1/2 in the sorted set of reduced
proper fractions for d <= 12,000?
-}

-- Farey Sequence; credits go to the PE73 forum for the idea
-- Also see http://mathworld.wolfram.com/FareySequence.html

-- Construct the Farey sequence for a/b to c/d with some bound
farey :: Int -> Int -> Int -> Int -> Int -> Int
farey a b c d bound
    | b' > bound = 0
    | otherwise = 1 + farey a  b  a' b' bound
                    + farey a' b' c  d  bound
    where
        a' = a + c
        b' = b + d

-- Print and write out the answer
main :: IO()
main = do
        let ans = farey 1 3 1 2 12000
        writeFile "pe73.txt" $ show ans
        print ans
