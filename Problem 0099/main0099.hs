{-
Using base_exp.txt , a 22K text file containing one thousand lines with a base/exponent 
pair on each line, determine which line number has the greatest numerical value.
-}

-- Since y=ln(x) is a monotonically increasing function, then a < b <<->> ln(a) < ln(b).
-- This makes our computation fairly easy

import Data.List
import Data.Maybe

-- Define an input line parse function
parse :: String -> (Integer, Integer)
parse = read . ('(' :) . (++ ")")

-- Define a fuction to transform a base/exp pair into its ln value
lnVal :: (Integer,Integer) -> Float
lnVal p@(base,exp) = (fromIntegral exp) * log (fromIntegral base)

-- Print and write out the answer
main = do
		pairs <- fmap ((map parse) . lines) $ readFile "base_exp.txt" :: IO [(Integer, Integer)]
		let lnNums = [lnVal ps | ps <- pairs]
		let maxLnNum = maximum lnNums
		let ans = 1 + fromJust (findIndex (==maxLnNum) lnNums) 
		writeFile "pe99.txt" $ show ans
		print ans