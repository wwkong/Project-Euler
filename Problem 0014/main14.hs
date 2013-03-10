import Data.List
import Data.Bits
import qualified Data.MemoCombinators as Memo

-- FAILED. Full credit goes to Xavier Shay

collatz :: Integer -> Integer
collatz = Memo.arrayRange (1,1000000) collatz'
  where
    collatz' 1  = 1
    collatz' x
      | even x    = 1 + collatz (x `shiftR` 1)
      | otherwise = 1 + collatz (3 * x + 1)

maxIndex = snd . foldl1' max . (flip zip [0..])
main = print $ (1 + (maxIndex $ map collatz [1.. 1000000]))
	

