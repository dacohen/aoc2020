import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, eof, sepBy1, manyTill)
import Text.Parsec.Char (anyChar, letter, char, space, digit, endOfLine)
import qualified Data.Map as Map

data BagRule = BagRule { parentColor :: String, childCounts :: [(String, Int)] } deriving (Show)

bagExpr :: Parser String
bagExpr = do
    bagColor <- manyTill anyChar (try $ string " bags")
    return bagColor

bagListItemExpr :: Parser (String, Int)
bagListItemExpr = do
    optional spaces
    count <- many1 digit
    space
    color <- manyTill anyChar ((try $ string " bags") <|> (try $ string " bag"))
    return (color, read count)

bagListEmptyExpr :: Parser [(String, Int)]
bagListEmptyExpr = do
    string "no other bags"
    return []

bagListExpr :: Parser [(String, Int)]
bagListExpr = do
    items <- sepBy1 bagListItemExpr (char ',')
    char '.'
    return items

bagRuleExpr :: Parser BagRule
bagRuleExpr = do
    parentColor <- bagExpr
    string " contain "
    childCounts <- bagListExpr <|> bagListEmptyExpr
    return $ BagRule { parentColor = parentColor, childCounts = childCounts }

parseLine :: String -> BagRule
parseLine line = case parse bagRuleExpr "" line of
    Left _ -> error "Invalid rule."
    Right a -> a

loadRules :: FilePath -> IO [BagRule]
loadRules path = do
    contents <- readFile path
    return $ map parseLine (lines contents)


nestedParents :: [BagRule] -> String -> [String]
nestedParents rules current =
    Map.keys elemMap
    where
        parentColors = map parentColor $ parentBags rules current
        allColors = concatMap (\color -> nestedParents rules color) parentColors ++ parentColors
        elemMap = Map.fromList (map (\e -> (e, 1)) allColors)

parentBags :: [BagRule] -> String -> [BagRule]
parentBags rules target =
    filter (\rule -> elem target (map (\pair -> fst pair) (childCounts rule))) rules

main = do
    rules <- loadRules "input"
    print $ length $ nestedParents rules "shiny gold"