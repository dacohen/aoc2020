import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, eof)
import Text.Parsec.Char (letter, char, digit, endOfLine)
import qualified Data.Map as Map

answerExpr :: Parser Char
answerExpr = do
    answer <- letter
    optional endOfLine
    return answer

groupExpr :: Parser [Char]
groupExpr = do
    group <- many1 answerExpr
    endOfLine
    optional endOfLine
    return group

fileExpr :: Parser [[Char]]
fileExpr = do
    file <- many1 groupExpr
    eof
    return file


parseFile :: String -> [[Char]]
parseFile input = case parse fileExpr "" input of
    Left _ -> []
    Right a -> a

getGroups :: FilePath -> IO [[Char]]
getGroups path = do
    contents <- readFile path
    return $ parseFile contents


countUnique :: String -> Int
countUnique input =
    length $ Map.keys elemMap
    where
        elemMap = Map.fromList (map (\e -> (e, 1)) input)


main = do
    groups <- getGroups "input"
    print $ sum (map countUnique groups)