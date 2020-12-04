import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, eof)
import Text.Parsec.Char (letter, char, digit, endOfLine)
import Data.Maybe (catMaybes)
import qualified Data.List as List

type Passport = [(String, String)]

dataField :: Parser (String, String)
dataField = do
    key <- many1 (letter <|> digit)
    char ':'
    value <- many1 (letter <|> digit <|> char '#')
    skipMany (satisfy (==' '))
    optional endOfLine
    return (key, value)

passportExpr :: Parser Passport
passportExpr = do
    fields <- many1 dataField
    endOfLine
    optional endOfLine
    return fields

passportFile :: Parser [Passport]
passportFile = do
    passports <- many1 passportExpr
    return passports


parseFile :: String -> [Passport]
parseFile input = case parse passportFile "" input of
    Left _ -> []
    Right a -> a


getPassports :: FilePath -> IO [Passport]
getPassports path = do
    contents <- readFile path
    return $ parseFile contents


isValid :: Passport -> Bool
isValid passport =
    (length $ List.intersect allFields (map (fst) passport)) == length allFields
    where allFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


countValids :: [Passport] -> Int
countValids passports = length (filter isValid passports)

main = do
    passports <- getPassports "input"
    print $ countValids passports