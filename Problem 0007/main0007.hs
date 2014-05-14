-- What is the 10 001st prime number?

-- We use the built in prime number generator as a hack ;D
import           Data.Numbers.Primes

-- Print and write out the answer
main :: IO()
main = do
        let ans = primes !! (10001 - 1) :: Integer -- Done to handle Haskell indexing
        writeFile "pe7.txt" $ show ans
        print ans
