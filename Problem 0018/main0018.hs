-- Find the maximum total from top to bottom of the triangle in data.txt.

-- Thanks to a tip from Stack Exchange

parseTree :: String -> [[Int]]
parseTree s = reverse $ map (map read) $ map words $ lines s

findPath :: [[Int]] -> Int
findPath tree
    | length (tree !! 0) == 1 = tree !! 0 !! 0
    | otherwise       = findPath $ [(zipWith (+)
                        [max (tree !! 0 !! n) (tree !! 0 !! (n+1)) | n <- [0..((length (tree !! 0))-1)]]
                        (tree !! 1))] ++ tail (tail tree)

main :: IO()
main = do
    contents <- readFile "data.txt"
    let tData = parseTree contents
    let ans = findPath tData
    writeFile "pe18.txt" $ show ans
    print ans
