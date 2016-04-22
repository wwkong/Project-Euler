{-
It turns out that 12 cm is the smallest length of wire that can be bent to form an integer sided right angle triangle in exactly one way, but there are many more examples.

12 cm: (3,4,5)
24 cm: (6,8,10)
30 cm: (5,12,13)
36 cm: (9,12,15)
40 cm: (8,15,17)
48 cm: (12,16,20)

In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle triangle, and other lengths allow more than one solution to be found; for example, using 120 cm it is possible to form exactly three different integer sided right angle triangles.

120 cm: (30,40,50), (20,48,52), (24,45,51)

Given that L is the length of the wire, for how many values of L ≤ 1,500,000 can exactly one integer sided right angle triangle be formed?
-}

import           Data.List

-- Pythagorean de-duping and filtering function which also calculates the length of the filtered list
pDedup :: [Int] -> Int
pDedup = length . filter (\l -> length l == 1) . group . sort

-- Generate all Pythagorean doubles (a,b) [with the lengths] less than a given length using a=m^2-n^2, b=2*m*n, c=m^2+n^2
-- and only output the lengths
-- Note that the smallest a is 3, for m=2, n=1 so use the restriction m < intRoot(l/2 - 2*1)
pPythag :: Int -> [Int]
pPythag len = tList >>= (\([a,b],l) -> [l*k | k <- [1..len `div` l]]) where
    maxM = (floor . sqrt) (fromIntegral len/2 - 2)
    tList = [([a,b],2*m*(m+n)) |  m <- [2..maxM], n <- [1..(m-1)],
                                        let a = m^2 - n^2, let b = 2*m*n,
                                        gcd m n == 1, odd (m-n)]

-- Print and write
main :: IO()
main = do
        let ans = (pDedup . pPythag) 1500000
        writeFile "pe75.txt" $ show ans
        print ans
