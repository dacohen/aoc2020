import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, eof)
import Text.Parsec.Char (letter, char, digit, endOfLine)
import Data.List

type Answer = [Char]

answerExpr :: Parser Answer
answerExpr = do
    answer <- many1 letter
    endOfLine
    return answer

groupExpr :: Parser [Answer]
groupExpr = do
    group <- many1 answerExpr
    endOfLine
    return group

fileExpr :: Parser [[Answer]]
fileExpr = do
    file <- many1 groupExpr
    eof
    return file


parseFile :: String -> [[Answer]]
parseFile input = case parse fileExpr "" input of
    Left _ -> []
    Right a -> a

getGroups :: FilePath -> IO [[Answer]]
getGroups path = do
    contents <- readFile path
    return $ parseFile contents

countCommon :: [Answer] -> Int
countCommon group =
    length $ foldl intersect (head group) group

main = do
    groups <- getGroups "input"
    print $ sum (map countCommon groups)