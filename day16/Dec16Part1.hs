import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, optional, sepBy)
import Text.Parsec.Char (letter, char, digit, string, endOfLine)

type Range = (Int, Int) -- (Low, High)
type MultiRange = [Range]
type Rule = (String, MultiRange) -- (Name, Range)
type Ticket = [Int]
data File = File { rules :: [Rule], yourTicket :: Ticket, nearbyTickets :: [Ticket] } deriving (Show)

plainSpace :: Parser Char
plainSpace = satisfy (==' ')

rangeExpr :: Parser Range
rangeExpr = do
    low <- many1 digit
    char '-'
    high <- many1 digit
    return $ (read low, read high)

multiRangeExpr :: Parser MultiRange
multiRangeExpr = do
    ranges <- sepBy1 rangeExpr (string " or ")
    return ranges

ruleExpr :: Parser Rule
ruleExpr = do
    name <- many1 (letter <|> plainSpace)
    char ':'
    plainSpace
    ranges <- multiRangeExpr
    endOfLine
    return (name, ranges)

rulesExpr :: Parser [Rule]
rulesExpr = do
    rules <- many1 ruleExpr
    return rules


ticketExpr :: Parser Ticket
ticketExpr = do
    values <- sepBy1 (many1 digit) (char ',')
    endOfLine
    return (map read values)

yourTicketExpr :: Parser Ticket
yourTicketExpr = do
    string "your ticket:"
    endOfLine
    ticket <- ticketExpr
    return ticket

nearbyTicketsExpr :: Parser [Ticket]
nearbyTicketsExpr = do
    string "nearby tickets:"
    endOfLine
    tickets <- many1 ticketExpr
    return tickets


fileExpr :: Parser File
fileExpr = do
    rules <- rulesExpr
    endOfLine
    yourTicket <- yourTicketExpr
    endOfLine
    nearbyTickets <- nearbyTicketsExpr
    return $ File { rules = rules, yourTicket = yourTicket, nearbyTickets = nearbyTickets }

parseFile :: String -> File
parseFile input = case parse fileExpr "" input of
    Left err -> error ("Unable to parse file: " ++ show err)
    Right a -> a

getFile :: FilePath -> IO File
getFile path = do
    contents <- readFile path
    return $ parseFile contents


inRange :: MultiRange -> Int -> Bool
inRange multiRange val =
    any (== True) [ val >= (fst range) && val <= (snd range) | range <- multiRange ]


invalidValues :: [Rule] -> Ticket -> [(Int, Int)] -- (Idx, Value)
invalidValues ruleList ticket =
    map (\(idx, _) -> (idx, ticket !! idx)) $ filter (not . snd) $ zip [0..] [ any (id) [ inRange (snd rule) field | rule <- ruleList ] | field <- ticket ]

main = do
    file <- getFile "input"
    let invalids = map (invalidValues (rules file)) (nearbyTickets file)
    print $ sum $ map (snd) (concat invalids)