-- Get the power set of a set of integers, excluding the empty set
powerset :: [Int] -> [[Int]]
powerset = init . filterM (const [True, False])