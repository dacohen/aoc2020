getInts :: FilePath -> IO [Int]
getInts path = do
    contents <- readFile path
    let floats = map read . lines $ contents
    return floats

uniqueTriples :: [Int] -> [[Int]]
uniqueTriples l = [[x, y, z] | x <- l, y <- l, z <- l, x < y && y < z]


solveProblem :: [Int] -> Int
solveProblem ints =
    let magicTriple = (filter (\triple -> foldl (+) 0 triple == 2020) . uniqueTriples $ ints) !! 0
    in foldl (*) 1 magicTriple

main = do
    ints <- getInts "input"
    print $ solveProblem ints
