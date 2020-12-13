import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (letter, char, digit, space)

data Action = North Double | South Double | East Double | West Double | TurnLeft Double | TurnRight Double | Forward Double deriving (Show)
data Ship = Ship { angle :: Double, east :: Double , north :: Double } deriving (Show)

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


applyAction :: Ship -> Action -> Ship
applyAction ship action =
    case action of
        North offset -> Ship { angle = angle ship, east = east ship, north = north ship + offset }
        South offset -> Ship { angle = angle ship, east = east ship, north = north ship - offset }
        East offset -> Ship { angle = angle ship, east = east ship + offset, north = north ship }
        West offset -> Ship { angle = angle ship, east = east ship - offset, north = north ship }
        TurnLeft offset -> Ship { angle = angle ship + offset, east = east ship, north = north ship }
        TurnRight offset -> Ship { angle = angle ship - offset, east = east ship, north = north ship }
        Forward offset -> Ship { angle = angle ship, east = east ship + (cos (radians $ angle ship)) * offset, north = north ship + (sin $ ((radians $ angle ship))) * offset }

applyActions :: [Action] -> Ship -> Ship
applyActions [] ship = ship
applyActions (action:list) ship =
    applyActions list (applyAction ship action)


manhattanDistance :: Ship -> Double
manhattanDistance ship =
    abs (east ship) + abs (north ship)

main = do
    actions <- getActions "input"
    let finalShip = applyActions actions Ship { angle = 0, east = 0, north = 0 }
    print $ finalShip
    print $ manhattanDistance finalShip