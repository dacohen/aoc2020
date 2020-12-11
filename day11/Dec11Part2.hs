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


rays :: Grid -> (Int, Int) -> [[(Int, Int)]]
rays grid (startRow, startCol) =
    [northRay, southRay, westRay, eastRay, neRay, seRay, swRay, nwRay]
    where
        maxDim = max (height grid) (width grid)
        northRay = [(row, startCol) | row <- reverse [0..startRow-1]]
        southRay = [(row, startCol) | row <- [startRow+1..height grid-1]]
        westRay = [(startRow, col) | col <- reverse [0..startCol-1]]
        eastRay = [(startRow, col) | col <- [startCol+1..width grid-1]]
        neRay = [(startRow-counter, startCol+counter) | counter <- [1..maxDim-1], startRow - counter >= 0, startCol + counter < width grid]
        seRay = [(startRow+counter, startCol+counter) | counter <- [1..maxDim-1], startRow + counter < height grid, startCol + counter < width grid]
        swRay = [(startRow+counter, startCol-counter) | counter <- [1..maxDim-1], startRow + counter < height grid, startCol - counter >= 0]
        nwRay = [(startRow-counter, startCol-counter) | counter <- [1..maxDim-1], startRow - counter >= 0, startCol - counter >= 0]


isOccupiedFirst :: Grid -> [Seat] -> Bool
isOccupiedFirst grid [] = False
isOccupiedFirst grid (seat:seats) =
    case seat of
        Floor -> isOccupiedFirst grid seats
        Occupied -> True
        Empty -> False

visibleCount :: Grid -> (Int, Int) -> Int
visibleCount grid pos =
   length $ filter (\seatRay -> isOccupiedFirst grid seatRay) seatLists
    where
        seatLists = map (map (seatAt grid)) (rays grid pos)


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
        emptyRule = visibleCount grid pos == 0
        occupiedRule = visibleCount grid pos >= 5


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