getSeatCodes :: FilePath -> IO [String]
getSeatCodes path = do
    contents <- readFile path
    return $ lines contents


partition :: (Int, Int) -> [Char] -> Int
partition (x, y) [] = x
partition (low, high) (letter:code)
    | elem letter ['F', 'L'] = partition (low, low + halfWidth) code
    | elem letter ['B', 'R'] = partition (low + halfWidth + 1, high) code
    where halfWidth = (high - low) `div` 2

getSeatId :: String -> Int
getSeatId code =
    rowNum * 8 + colNum
    where
        rowNum = partition (0, 127) (take 7 code)
        colNum = partition (0, 7) (drop 7 code)

missingSeatId :: [Int] -> Int
missingSeatId takens =
    head [x | x <- [low..high], not $ elem x takens ]
    where
        low = minimum takens
        high = maximum takens


main = do
    seatCodes <- getSeatCodes "input"
    print $ missingSeatId (map getSeatId seatCodes)