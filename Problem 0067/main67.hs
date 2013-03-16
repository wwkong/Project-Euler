-- Same as Problem 18
import Data.List
import System.IO

parseTree :: String -> [[Int]]
parseTree s = reverse $ map (map read) $ map words $ lines s

findPath :: [[Int]] -> Int
findPath tree
	| length (tree !! 0) == 1 = tree !! 0 !! 0
	| otherwise				  = findPath $ [(zipWith (+) 
										             [max (tree !! 0 !! n) (tree !! 0 !! (n+1)) | n <- [0..((length (tree !! 0))-1)]] 
												     (tree !! 1))] ++ tail (tail tree)											
main = do
	contents <- readFile "triangle.txt" 
	let tData = parseTree contents
	return $ findPath tData
	