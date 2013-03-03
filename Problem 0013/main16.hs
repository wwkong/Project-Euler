import Data.Char

main = do 
		pe16data <- readFile "data.csv"
		let pe16str = words pe16data
		let pe16num = map (read::String->Integer) pe16str
		print $ take 10 $ show $ (sum::[Integer] -> Integer) pe16num 