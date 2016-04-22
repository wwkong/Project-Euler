{-
It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.

Total   Solution Set
9   4,2,3; 5,3,1; 6,1,2
9   4,3,2; 6,2,1; 5,1,3
10  2,3,5; 4,5,1; 6,1,3
10  2,5,3; 6,3,1; 4,1,5
11  1,4,6; 3,6,2; 5,2,4
11  1,6,4; 5,4,2; 3,2,6
12  1,5,6; 2,6,4; 3,4,5
12  1,6,5; 3,5,4; 2,4,6

By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.

Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings. What is the maximum 16-digit string for a "magic" 5-gon ring?
-}

import Data.List
import Data.Char

-- Select subsets of size k
choose :: [Int] -> Int -> [[Int]]
choose _      0 =  [[]]
choose []     _ =  []
choose (x:xs) k =  fmap (x:) (choose xs (k-1)) ++ choose xs k

-- Taken from the StackExchange, this function converts an integer list to a string
intToString :: [Int] -> String
intToString = concatMap show

-- Takes an inner n-gon configuration and n and checks if outside nodes can be added to make it a magic n-gon ring
isMagic :: Int -> [Int] -> Bool
isMagic n cfg = all (== head matchSums) (tail matchSums)
    where
        iCfg = cycle cfg
        cCfg = [1..(2*n)] \\ cfg
        pairSums = take n (zipWith (+) iCfg (tail iCfg))
        matchSums = zipWith (+) (sort pairSums) (sortBy (flip compare) cCfg)

-- Takes an inner n-gon configuration and n and returns the string representation
getMagicString :: Int -> [Int] -> String
getMagicString n cfg = intToString $ concat orderedEdges
    where
        iCfg = cycle cfg
        cCfg = [1..(2*n)] \\ cfg
        pairs = take n (zipWith (\a b -> [a,b]) iCfg (tail iCfg))
        pairSums = map (\[a,b] -> a+b) pairs
        ngSum = maximum pairSums + minimum cCfg
        edges = map (\[a,b] -> [ngSum-a-b, a, b]) pairs
        orderedEdges = take n (dropWhile (\l -> head l /= minimum cCfg) (cycle edges))

-- Finds the list of strings of n-gon configurations with the appropriate orientation given n
getMagicList :: Int -> [String]
getMagicList n = nub $ map (getMagicString n) innerElems
    where
        innerElems =  filter (isMagic n) pElems
        pElems = concatMap permutations (choose [1..(2*n)] n)

-- Test it on the given example
test :: Int
test = maximum (map (read :: String -> Int) (getMagicList 3))

-- Print and write
main :: IO()
main = do
        let ans = maximum (map (read :: String -> Int) (filter (\s -> length s == 16) (getMagicList 5)))
        writeFile "pe68.txt" $ show ans
        print ans
