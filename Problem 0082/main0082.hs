{-
In the 5 by 5 matrix below, the minimal path sum from the left to the right,
by only moving to the right, up, and down, is indicated in bold red and is equal to 994.
131  673  234. 103. 18.
201. 96.  342. 965  150
630  803  746  422  111
537  699  497  121  956
805  732  524  37   331
Find the minimal path sum, in p082_matrix.txt, a 31K text file containing a 80 by
80 matrix, from the top left to the bottom right by only moving right, up, and down.
-}

import           Data.List.Split
import           Data.List
import qualified Data.Vector as V

-- We use a method similar to Problem #81 (use O(1) vector indexing)

-- One step algorithm first
findMinPathOne :: V.Vector Int -> V.Vector Int -> V.Vector Int
findMinPathOne v1 v2
    | V.null v1 || V.null v2 = V.empty
    | otherwise = V.fromList [ minPath i | i <- [1.. V.length v2] ]
        where   minPath i =
                    V.minimum $ foldMinSum (v2 V.! (i-1))
                                           (V.fromList [ v12 V.! (n-1) | let m = max (i-1) 1, n <- [m, (m-1) .. 1], n /= i]) V.++
                                V.fromList [v1 V.! (i-1) + v2 V.! (i-1)]  V.++
                                foldMinSum (v2 V.! (i-1))
                                           (V.fromList [ v12 V.! (n-1) | n <- [min (i+1) (V.length v1) .. V.length v1], n /= i])
                foldMinSum b v =
                    V.zipWith (+) (V.map fst v) $ V.tail $ V.scanl (\b v -> b + snd v) b v
                v12 = V.zip v1 v2

-- Fold over the matrix columns
findMinPath :: V.Vector (V.Vector Int) -> Int
findMinPath inMatrix =
    V.minimum $ V.foldl1 findMinPathOne inMatrix

-- Create a matrix parsing function
readMat :: String -> [[Int]]
readMat = (map (map (read :: String -> Int)) . map (splitOn "," )) . words

-- Create a matrix to nested vector conversion function
convMat :: [[Int]] -> V.Vector (V.Vector Int)
convMat m =  V.fromList $ map V.fromList m

-- Try out our test case (it works!~)
testMatrix :: [[Int]]
testMatrix = transpose [    [131, 673, 234, 103, 18 ],
                            [201, 96 , 342, 965, 150],
                            [630, 803, 746, 422, 111],
                            [537, 699, 497, 121, 956],
                            [805, 732, 524, 37 , 331]  ]
test = findMinPath $ convMat testMatrix

-- Print and write
main :: IO()
main = do
        rawMatrix <- readFile "p082_matrix.txt"
        let parsedMatrix = readMat rawMatrix
        let ans = findMinPath $ (convMat . transpose) parsedMatrix
        writeFile "pe82.txt" $ show ans
        print ans
