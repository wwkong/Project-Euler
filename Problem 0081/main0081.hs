{-
In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
by only moving to the right and down, is indicated in bold red and is equal to 2427.

131. 673  234  103  18
201. 96.  342. 965  150
630  803  746. 422. 111
537  699  497  121. 956
805  732  524  37.  331.

Find the minimal path sum, in matrix.txt, a 31K text file containing a 80 by
80 matrix, from the top left to the bottom right by only moving right and down.
-}

import           Data.List.Split

-- Credit goes to the Haskell wiki for the inspiration

-- We use a method similar to Problem #67
findMinPath :: [[Int]] -> Int
findMinPath grid = last $ (foldl accUp) (scanl1 (+) (head grid)) (tail grid) where
    accUp (u:ut) (n:nt) = scanl accRight (u+n) (zip ut nt) where
        accRight leftAcc (upAcc,next) = next + min leftAcc upAcc


-- Try out our test case (it works!~)
testMatrix :: [[Int]]
testMatrix = [  [131, 673, 234, 103, 18 ],
                [201, 96 , 342, 965, 150],
                [630, 803, 746, 422, 111],
                [537, 699, 497, 121, 956],
                [805, 732, 524, 37 , 331]  ]
test :: Int
test = findMinPath testMatrix

-- Create a matrix parsing function
readMat :: String -> [[Int]]
readMat =  (map (map (read :: String -> Int))) . (map (splitOn "," )) . words

-- Print and write
main :: IO()
main = do
        rawMatrix <- readFile "matrix.txt"
        let parsedMatrix = readMat rawMatrix
        let ans = findMinPath parsedMatrix
        writeFile "pe81.txt" $ show ans
        print ans
