-- Will be revisited because I will be doing this by hand

-- Given that the three characters are always asked for in order, analyse the file keylog.txt so as to 
-- determine the shortest possible secret passcode of unknown length.

import Data.List
import Debug.Trace
import qualified Data.Set as S

-- Define an ordered union operator for two list elements (B into A)
ordUnion :: String -> String -> String
ordUnion lstA lstB = ordUnion' lstA lstB [] where
	ordUnion' lstA' lstB' acc
		| lstB' == [] = acc
		| trace ("bMatch=" ++ show bMatch ++ " restA=" ++ show restA) False = undefined
		| otherwise = ordUnion' restA (tail lstB') (acc ++ (union [head lstB'] lstA'))
		where 
			bMatch = head $ (findIndices (==(head lstB')) lstA' ++ [(length lstA')-1])
			restA = snd $ splitAt bMatch lstA'

-- Given a list of ordered strings, define a function that merges them in order
mergeStrLst strLst = foldl1 union strLst

-- Implement a tuple min function
-- Create a tuple max function
tupMin a b
	| min (fst a) (fst b) == fst a = a
	| min (fst a) (fst b) == fst b = b

-- Print and write out the answer
main = do
		--rawData <- readFile "keylog.txt"
		--let contents = (S.toList . S.fromList . lines) rawData -- Remove duplicates
		let ans = -- (read :: String -> Integer) $ mergeStrLst contents
		writeFile "pe79.txt" $ show ans
		print ans