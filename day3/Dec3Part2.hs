data Grid = Grid { width :: Int, height :: Int, value :: [[Bool]] } deriving (Show)

getGrid :: FilePath -> IO Grid
getGrid path = do
    contents <- readFile path
    return Grid { width = length $ head (lines contents)
                , height = length $ lines contents
                , value = [ map (\char -> if char == '#' then True else False) xs | xs <- lines contents ]
                }

-- Tuple is (rows, cols)
generatePosSequence :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
generatePosSequence dims vector =
    [((counter * down) + 1, (((counter * right) `mod` cols) + 1)) | counter <- [0..(rows `div` down)-1]]
    where rows = fst dims
          cols = snd dims
          down = fst vector
          right = snd vector

generateSuperSequence :: (Int, Int) -> [(Int, Int)] -> [[(Int, Int)]]
generateSuperSequence dims vectors = map (generatePosSequence dims) vectors

countTrees :: Grid -> [(Int, Int)] -> Int
countTrees grid posSequence =
    length $ filter (\pos -> (value grid) !! ((fst pos) - 1) !! ((snd pos) - 1)) posSequence

main = do
    grid <- getGrid "input"
    print $ foldl (*) 1 $ map (countTrees grid) (generateSuperSequence (height grid, width grid) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)])
