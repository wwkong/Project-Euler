-- We first note that the upper limit is 999999 (six digts)

import Data.Char 

-- Define a function to return the sum of the fifth powered digits
fPwr n = sum $ map (^5) $ map digitToInt $ show n 

-- Define a function to count the number of summed digit fifth powers equal
-- to the original given a limit
sumFPwr lim = sumFPwr' 2 lim 0 where -- Disregard 0 and 1
	sumFPwr' n lim sumTot
		| n > lim = sumTot
		| sumF == n = sumFPwr' (n+1) lim (sumTot + sumF)
		| otherwise = sumFPwr' (n+1) lim sumTot
		where 
			sumF = fPwr n 

main = do
		let ans = sumFPwr 999999
		writeFile "pe30.txt" $ show ans
		print ans