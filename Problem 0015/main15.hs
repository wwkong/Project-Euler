-- This is more of a combinatorics problem than a programming one (20x20 grid has 40 C 20 combinations)
choose n 0 = 1
choose n k = ((choose (n-1) (k-1)) * n) `div` k 
main = print (choose 40 20)
