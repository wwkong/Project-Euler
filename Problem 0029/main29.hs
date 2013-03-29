-- First application of functors and the Data.Set package
-- Took 0.06s to compute.

import Control.Applicative
import Data.Set

-- Print and write out the result
main = do
		let perm = (^) <$> [2..100] <*> [2..100]
		let ans = size $ fromList perm
		writeFile "pe29.txt" $ show ans
		print ans