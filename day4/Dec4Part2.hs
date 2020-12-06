import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, eof)
import Text.Parsec.Char (letter, char, digit, oneOf, endOfLine)
import Text.Parsec.Token (identifier)
import Data.Maybe (catMaybes)
import qualified Data.List as List

type Passport = [(String, String)]

heightExpr :: Parser (Int, String)
heightExpr = do
    height <- many1 digit
    unit <- string "cm" <|> string "in"
    eof
    return (read height, unit)


hairExpr :: Parser String
hairExpr = do
    char '#'
    color <- count 6 (digit <|> oneOf "abcdef")
    eof
    return color


passportIdExpr :: Parser Int
passportIdExpr = do
    number <- count 9 digit
    eof
    return (read number)


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
    ((length $ List.intersect allFields (map (fst) passport)) == length allFields)
    && all validateField passport
    where allFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


validateField :: (String, String) -> Bool
validateField tuple
    | key == "byr" = numValue >= 1920 && numValue <= 2002
    | key == "iyr" = numValue >= 2010 && numValue <= 2020
    | key == "eyr" = numValue >= 2020 && numValue <= 2030
    | key == "hgt" =
        case (parse heightExpr "" value) of
            Left _ -> False
            Right (size, unit) -> case unit of
                "cm" -> size >= 150 && size <= 193
                "in" -> size >= 59 && size <= 76
    | key == "hcl" =
        case (parse hairExpr "" value) of
            Left _ -> False
            Right _ -> True
    | key == "ecl" = elem value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    | key == "pid" =
        case (parse passportIdExpr "" value) of
            Left _ -> False
            Right _ -> True
    | otherwise    = True
    where
        key = fst tuple
        value = snd tuple
        numValue = read value


countValids :: [Passport] -> Int
countValids passports = length (filter isValid passports)

main = do
    passports <- getPassports "input"
    print $ countValids passports