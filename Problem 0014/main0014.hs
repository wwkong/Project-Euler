-- Which starting number, in the Collatz sequence under one million, produces the longest chain?

import qualified Data.List.Stream as S
import           Data.Word

-- Credit goes to http://www.mit.edu/~mtikekar/posts/stream-fusion.html for the stream fusion
-- optimization idea and the Data.Word package mention

-- Define the function which returns the next Collatz number
nextCollatz :: Word -> Word
nextCollatz n = (if even n then n else 3*n+1) `div` 2

-- Define the function which calculates the length of a Collatz sequence
lenCollatz :: Word -> Int
lenCollatz n = S.length $ S.takeWhile (/=1) $ S.iterate nextCollatz n

-- Print and write out the answer
main :: IO()
main = do
		let ans = S.maximum $ S.map (\n -> (lenCollatz n, n)) [1.. (10^6-1)]
		writeFile "pe14.txt" $ show ans
		print ans
