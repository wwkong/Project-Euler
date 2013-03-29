-- Took 0.03s to compute.

-- Same as Problem 18
import Data.List

-- Implement a reverse tree finder
findPath :: [[Int]] -> Int
findPath tree
	| length (tree !! 0) == 1 = tree !! 0 !! 0
	| otherwise				  = findPath $ [(zipWith (+) 
										             [max (tree !! 0 !! n) (tree !! 0 !! (n+1)) | n <- [0..((length (tree !! 0))-1)]] 
												     (tree !! 1))] ++ tail (tail tree)	

-- Print and write out the answer													 
main = do
	rawContents <- readFile "triangle.txt" 
	let mContents = reverse $ map (map read) $ map words $ lines rawContents
	let ans = findPath mContents
	writeFile "pe67.txt" $ show ans
	print ans