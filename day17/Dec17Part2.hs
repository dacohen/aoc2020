import qualified Data.Map as Map

type Vec4 = (Int, Int, Int, Int)
type Grid = Map.Map Vec4 Bool


getGrid :: FilePath -> IO Grid
getGrid path = do
    contents <- readFile path
    return $ Map.fromList $ concat [ map (\(x, char) -> case char of
                                                            '.' -> ((x, y, 0, 0), False)
                                                            '#' -> ((x, y, 0, 0), True)) (zip [0..] line) 
                                    | (y, line) <- zip [0..] $ lines contents ]


adjacent :: Vec4 -> [Vec4]
adjacent (startX, startY, startZ, startW) =
    [(x, y, z, w) | 
    x <- [startX-1..startX+1],
    y <- [startY-1..startY+1],
    z <- [startZ-1..startZ+1],
    w <- [startW-1..startW+1],
    (x, y, z, w) /= (startX, startY, startZ, startW)]


applyRule :: Grid -> Vec4 -> (Vec4, Bool)
applyRule grid pos =
    if isActive && (elem activeNeighbors [2, 3]) then
        (pos, True)
    else if not isActive && (activeNeighbors == 3) then
        (pos, True)
    else
        (pos, False)
    where
        neighbors = adjacent pos
        isActive = case Map.lookup pos grid of
                        Nothing -> False
                        Just active -> active
        activeNeighbors = length $ filter (\loc -> case Map.lookup loc grid of
                                            Nothing -> False
                                            Just active -> active) neighbors


applyRules :: Grid -> Grid
applyRules grid =
    foldl (\newGrid (k, v) -> Map.insert k v newGrid) grid updateList
    where
        updateList = concat [ [ applyRule grid pos | pos <- adjacent cubePos ++ [cubePos] ] | cubePos <- Map.keys (Map.filter id grid) ]


runRounds :: Grid -> Int -> Grid
runRounds grid rounds =
    foldl (\newGrid _ -> applyRules newGrid) grid [1..rounds]

main = do
    grid <- getGrid "input"
    let finalGrid = runRounds grid 6
    print $ Map.size (Map.filter id finalGrid)