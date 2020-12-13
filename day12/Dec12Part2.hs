import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (letter, char, digit, space)

data Action = North Double | South Double | East Double | West Double | TurnLeft Double | TurnRight Double | Forward Double deriving (Show)
data Ship = Ship { angle :: Double, east :: Double , north :: Double } deriving (Show)
type Waypoint = (Double, Double) -- east, north

lineExpr :: Parser Action
lineExpr = do
    actionType <- letter
    value <- many1 digit
    return $ case actionType of
        'N' -> North $ read value
        'S' -> South $ read value
        'E' -> East $ read value
        'W' -> West $ read value
        'L' -> TurnLeft $ read value
        'R' -> TurnRight $ read value
        'F' -> Forward $ read value


radians :: Double -> Double
radians angle = angle * (pi / 180.0)

parseLine :: String -> Action
parseLine input = case parse lineExpr "" input of
    Left _ -> error "Invalid line."
    Right a -> a

getActions :: FilePath -> IO [Action]
getActions path = do
    contents <- readFile path
    return $ map parseLine (lines contents)

applyAction :: (Waypoint, Ship) -> Action -> (Waypoint, Ship)
applyAction (wp, ship) action =
    case action of
        North offset -> ((wpEast, wpNorth + offset), ship)
        South offset -> ((wpEast, wpNorth - offset), ship)
        East offset -> ((wpEast + offset, wpNorth), ship)
        West offset -> ((wpEast - offset, wpNorth), ship)
        TurnLeft offset -> ((wpEast * cos (radians offset) - wpNorth * sin (radians offset), wpEast * sin (radians offset) + wpNorth * cos (radians offset)), ship)
        TurnRight offset -> ((wpEast * cos (-(radians offset)) - wpNorth * sin (-(radians offset)), wpEast * sin (-(radians offset)) + wpNorth * cos (-(radians offset))), ship)
        Forward offset -> (wp, Ship { angle = angle ship, east = east ship + wpEast * offset, north = north ship + wpNorth * offset })
    where
        wpEast = fst wp
        wpNorth = snd wp


applyActions :: [Action] -> (Waypoint, Ship) -> (Waypoint, Ship)
applyActions [] (wp, ship) = (wp, ship)
applyActions (action:list) (wp, ship) =
    applyActions list (applyAction (wp, ship) action)


manhattanDistance :: Ship -> Double
manhattanDistance ship =
    abs (east ship) + abs (north ship)

main = do
    actions <- getActions "input"
    let (finalWp, finalShip) = applyActions actions ((10, 1), Ship { angle = 0, east = 0, north = 0 })
    print $ finalShip
    print $ manhattanDistance finalShip