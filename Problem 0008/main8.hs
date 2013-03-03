import System.IO  
import Data.Char

-- Full crecit goes to OJ's rants

main = do
		pe8str <- readFile "data.txt"
		let pe8num = map digitToInt pe8str
		let chunks = c' pe8num 5 995
				where
				c' _ _ 0 = []
				c' l n c = (take n l) : c' (tail l) n (c - 1)
		print $ foldr1 max
			  $ map product chunks
			  