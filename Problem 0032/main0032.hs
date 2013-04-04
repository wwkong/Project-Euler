-- We are given that 39*186 = 7254 is a pandigital product

{- 
To begin, first note that the multiplicand and multiplier, together, must be of length 5 or 4
(this can be checked with the number 12345 or 9876 and placing a * as a partition)
-}

import Control.Monad
import Control.Applicative
import Data.List
import Data.Char
import qualified Data.Set as S

-- Create a function to generate the ordered powerset of a list of elementh size 4 or 5
permNums n = ((S.toList . S.fromList) $ map (take n) $ permutations "123456789")
mmNums = (permNums 4) ++ (permNums 5)

-- Check if a concatenated multiplicand and multiplier pair can produce a pandigital expression
-- and produce the product
getPanProd :: String -> Integer
getPanProd nStr = getPanProd' nStr 1 where
	getPanProd' nStr posn
		| posn == len = 1
		| sort (nStr ++ mProd) == "123456789" = read mProd :: Integer
		| otherwise = getPanProd' nStr (posn + 1)
		where 
			mSplit = splitAt posn nStr 
			mProd = show $ (read $ fst mSplit :: Integer) * (read $ snd mSplit :: Integer)
			len = length nStr

-- Get all unique products
ansProds = (S.toList . S.fromList) $ map getPanProd mmNums

-- Print and write out the answer
main = do 
		let ans = sum ansProds
		writeFile "pe32.txt" $ show ans
		print ans

