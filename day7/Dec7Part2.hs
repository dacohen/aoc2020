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


countNested :: BagRule -> Int
countNested rule =
    sum $ map snd (childCounts rule)


ruleMap :: [BagRule] -> Map.Map String BagRule
ruleMap rules =
    Map.fromList ruleTuples
    where
        ruleTuples = map (\rule -> (parentColor rule, rule)) rules

countAllBags :: [BagRule] -> String -> Int
countAllBags rules color =
    (sum $ map (\color -> (countMap Map.! color) * countAllBags rules color) (map fst $ childCounts currentRule)) + countNested currentRule
    where
        currentRuleMap = ruleMap rules
        currentRule = currentRuleMap Map.! color
        countMap = Map.fromList (childCounts currentRule)

main = do
    rules <- loadRules "input"
    print $ countAllBags rules "shiny gold"