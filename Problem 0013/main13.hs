-- Took 0.02s to compute.

import Data.Char

-- Print and write out the answer
main = do 
		rawContents <- readFile "data.csv"
		let mContents = map (read :: String -> Integer) $ words rawContents
		let ans = read $ take 10 $ show $ (sum :: [Integer] -> Integer) mContents :: Int
		writeFile "pe13.txt" $ show ans
		print ans