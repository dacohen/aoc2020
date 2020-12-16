import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, optional, sepBy)
import Text.Parsec.Char (letter, char, digit, string, endOfLine)
import qualified Data.Map as Map
import Data.List

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


validTicket :: [Rule] -> Ticket -> Bool
validTicket ruleList ticket =
    all id [ any id [ inRange (snd rule) field | rule <- ruleList ] | field <- ticket ]

validMapping :: (Int, Rule) -> [Ticket] -> Bool
validMapping (idx, rule) tickets =
    all id [ inRange (snd rule) (ticket !! idx) | ticket <- tickets ]


rulesForField :: [Ticket] -> [Rule] -> Int -> [Rule]
rulesForField tickets ruleList idx =
    map snd $ filter fst [ (validMapping (idx, rule) tickets, rule) | rule <- ruleList ]


findMapping :: [Ticket] -> Map.Map Int [Rule] -> Map.Map Int Rule -> Map.Map Int Rule
findMapping tickets mappings finalMapping =
    case Map.null mappings of
        True -> finalMapping
        False -> findMapping tickets newMapping newFinalMapping
    where
        nextTrivialIdx = head $ Map.keys (Map.filter (\m -> length m == 1) mappings)
        trivialRule = head $ mappings Map.! nextTrivialIdx
        stripRules ruleList = [ rule | rule <- ruleList, fst rule /= fst trivialRule ]
        newFinalMapping = Map.insert nextTrivialIdx trivialRule finalMapping
        newMapping = Map.map (\val -> stripRules val) (Map.delete nextTrivialIdx mappings)


applyOrdering :: Map.Map Int Rule -> Ticket -> [(String, Int)]
applyOrdering ordering ticket =
    [ (fst (ordering Map.! key), ticket !! key) | key <- Map.keys ordering ]


main = do
    file <- getFile "input"
    let valids = filter (validTicket (rules file)) (nearbyTickets file)
    let validMappings = Map.fromList $ map (\idx -> (idx, rulesForField valids (rules file) idx)) [0..length (rules file)-1]
    let finalMapping = findMapping valids validMappings Map.empty
    let yourTicketMapped = applyOrdering finalMapping (yourTicket file)
    print $ foldl (\accum val -> accum * snd val) 1 (filter (\entry -> isPrefixOf "departure" (fst entry)) yourTicketMapped)