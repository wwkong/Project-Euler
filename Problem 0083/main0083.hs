{-
In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
by moving to the left, right, up, and down, is indicated in bold red and is equal to 2297.
131. 673  234. 103.  18.
201. 96.  342. 965   150.
630  803  746  422.  111.
537  699  497  121.  956
805  732  524  37.   331.
Find the minimal path sum, in p083_matrix.txt, a 31K text file containing a 80 by
80 matrix, from the top left to the bottom right by only moving right, up, and down.
-}

import           Debug.Trace
import           Data.List.Split
import           Data.List
import qualified Data.Maybe  as M
import qualified Data.Vector as V

-- We use a method similar to Problem #82 (use O(1) vector indexing)

-- Create a function to do safe vector conversion
toPathVec :: Maybe (V.Vector Int) -> V.Vector Int
toPathVec Nothing = V.empty
toPathVec mVec = M.fromJust mVec

-- Copy the one step algorithm from PE 81 with a modification
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

-- Wrap the above two functions when given the left comparison tuples and right direction
findMinPathTwo :: V.Vector Int -> V.Vector Int -> V.Vector Int -> (Int, V.Vector Int)
findMinPathTwo nextL mAccL nextR
    | V.null nextL = ( 1, nextR) -- Case where we are at the beginning; go right
    | V.null nextR = (-1, newL ) -- Case where we are at the end      ; go left
    | V.or (V.zipWith (<) nextL mAccL) = (-1, newL) -- Case where it is better to backtrack
    | otherwise = (1, nextR) -- Remaining case
    where
        newL = V.zipWith min nextL mAccL

-- Wrap all functions above
findDir ::  V.Vector (V.Vector Int) -> V.Vector (V.Vector Int) -> V.Vector Int -> Int -> Int
findDir m mAcc acc idx
    | terminate      = V.last acc
    | otherwise      = findDir m mNext (snd next) (idx + fst next)
    where
        nextR     = findMinPathOne acc (toPathVec $ m V.!? (idx+1))
        nextL     = findMinPathOne acc (toPathVec $ m V.!? (idx-1))
        next      = findMinPathTwo nextL (toPathVec $ mAcc V.!? (idx-1)) nextR
        mNext     = V.update mAcc (V.fromList [(idx,acc)])
        terminate = V.and (V.zipWith (<=) (toPathVec $ mAcc V.!? (idx-1)) nextL) && V.null nextR -- At the far right

-- Find the minimal path via a dynamic pivot point
findMinPath :: V.Vector (V.Vector Int) -> Int
findMinPath m =
    findDir m' m' (V.head m') 0
    where m' = (V.singleton (V.scanl1 (+) (V.head m))) V.++ (V.tail m)

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
        rawMatrix <- readFile "p083_matrix.txt"
        let parsedMatrix = readMat rawMatrix
        let ans = findMinPath $ (convMat . transpose) parsedMatrix
        writeFile "pe83.txt" $ show ans
        print ans
