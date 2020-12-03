getInts :: FilePath -> IO [Int]
getInts path = do
    contents <- readFile path
    let floats = map read . lines $ contents
    return floats

uniquePairs :: [Int] -> [[Int]]
uniquePairs l = [[x, y] | x <- l, y <- l, x < y]

solveProblem :: [Int] -> Int
solveProblem ints =
    let magicPair = (filter (\pair -> foldl (+) 0 pair == 2020) . uniquePairs $ ints) !! 0
    in foldl (*) 1 magicPair

main = do
    ints <- getInts "input"
    print $ solveProblem ints
