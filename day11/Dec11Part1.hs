data Seat = Floor | Empty | Occupied deriving (Show, Eq)
data Grid = Grid { width :: Int, height :: Int, value :: [[Seat]] } deriving (Show, Eq)

isOccupied :: Seat -> Bool
isOccupied seat =
    case seat of
        Floor -> False
        Empty -> False
        Occupied -> True

isFloor :: Seat -> Bool
isFloor seat =
    case seat of
        Floor -> True
        Empty -> False
        Occupied -> False

-- Tuple is (rows, cols)
getGrid :: FilePath -> IO Grid
getGrid path = do
    contents <- readFile path
    return Grid { width = length $ head (lines contents)
                , height = length $ lines contents
                , value = [ map (\char -> case char of
                                            '.' -> Floor
                                            'L' -> Empty
                                            '#' -> Occupied) xs
                           | xs <- lines contents ]}


adjacent :: Grid -> (Int, Int) -> [(Int, Int)]
adjacent grid (centerRow, centerCol) =
    [(row, col) |
        row <- [centerRow-1..centerRow+1],
        col <- [centerCol-1..centerCol+1],
        row >= 0 && row < (height grid),
        col >= 0 && col < (width grid),
        (row, col) /= (centerRow, centerCol)]

allPositions :: Grid -> [(Int, Int)]
allPositions grid =
    [(row, col) | row <- [0..height grid-1], col <- [0..width grid-1]]

seatAt :: Grid -> (Int, Int) -> Seat
seatAt grid (row, col) =
    (value grid) !! row !! col

replaceSeatRow :: [Seat] -> Int -> Seat -> [Seat]
replaceSeatRow row col newSeat =
    take col row ++ [newSeat] ++ drop (col + 1) row

replaceSeat :: Grid -> (Int, Int) -> Seat -> Grid
replaceSeat grid (row, col) newSeat =
    Grid { width = width grid
         , height = height grid
         , value = take row gridData ++ [newRow] ++ drop (row + 1) gridData }
    where
        gridData = value grid
        newRow = replaceSeatRow (gridData !! row) col newSeat


applyRule :: Grid -> (Int, Int) -> Seat
applyRule grid pos =
    if validSeat && emptyRule then
        Occupied
    else if validSeat && occupiedRule then
        Empty
    else
        currentSeat
    where
        currentSeat = seatAt grid pos
        validSeat = not $ isFloor currentSeat
        emptyRule = all (\loc -> not (isOccupied $ seatAt grid loc)) (adjacent grid pos)
        occupiedRule = (length $ filter (\loc -> isOccupied $ seatAt grid loc) (adjacent grid pos)) >= 4


applyRules :: Grid -> Grid
applyRules initialGrid =
    foldl (\grid (pos, seat) -> replaceSeat grid pos seat) initialGrid mappedPositions
    where
        positions = allPositions initialGrid
        mappedPositions = zip positions (map (\pos -> applyRule initialGrid pos) positions)


runRounds :: Grid -> Grid
runRounds grid =
    case grid == newGrid of
        True -> newGrid
        False -> runRounds newGrid
    where
        newGrid = applyRules grid

printRow :: [Seat] -> String
printRow row = [ case seat of
                    Empty -> 'L'
                    Floor -> '.'
                    Occupied -> '#'
                    | seat <- row ]

printGrid :: Grid -> String
printGrid grid =
    concatMap (\row -> printRow row ++ "\n") (value grid)


main = do
    grid <- getGrid "input"
    let finalGrid = runRounds grid
    putStr $ printGrid finalGrid
    print $ length (filter isOccupied (concat $ value finalGrid))