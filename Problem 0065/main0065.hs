{-
The sequence of the first ten convergents for 2 are:

1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
What is most surprising is that the important mathematical constant,
e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].

The first ten terms in the sequence of convergents for e are:

2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.

Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
-}

import           Data.Char
import           Data.Ratio

-- Generate the tail sequence for the continued fraction representation of e
tailESeq :: [Ratio Integer]
tailESeq = 1 : (concat $ map (\n -> [n,1,1]) [2*1,2*2..])

-- Import our function from Problem #57 and modify it a little bit

-- Define a function to find the n-th fractional expansion of e
sqrtEExp :: Int -> Ratio Integer
sqrtEExp n = 2 + sqrtEExp' 1 where
    sqrtEExp' :: Int -> Ratio Integer
    sqrtEExp' k
        | n == 1 = 0
        | (n-1) == k = 1 / (tailESeq !! (k-1))
        | otherwise = 1 / (tailESeq !! (k-1) + sqrtEExp' (k+1))

-- Print and write out the answer
main :: IO()
main = do
        let ans = (sum . (map digitToInt) . show) (numerator $ sqrtEExp 100)
        writeFile "pe65.txt" $ show ans
        print ans
