import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (letter, char, digit, space)
import Data.Maybe (catMaybes)

data Password = Password { minCount :: Int
                         , maxCount :: Int
                         , character :: Char
                         , password :: String
                         } deriving (Show)


-- Example line
-- 1-3 a: abcde

lineParser :: Parser Password
lineParser = do
    minCount <- many1 digit
    char '-'
    maxCount <- many1 digit
    many1 space
    character <- letter
    char ':'
    many1 space
    password <- many1 letter
    return Password { minCount = read minCount
                    , maxCount = read maxCount
                    , character = character
                    , password = password
                    }

parseLine :: String -> Maybe Password
parseLine input = case parse lineParser "" input of
    Left _ -> Nothing
    Right a -> Just a

getPasswords :: FilePath -> IO [Password]
getPasswords path = do
    contents <- readFile path
    return $ catMaybes (map parseLine (lines contents))


isValid :: Password -> Bool
isValid pwd =
    matcher leftLetter /= matcher rightLetter
    where leftLetter = (password pwd) !! (minCount pwd - 1)
          rightLetter = (password pwd) !! (maxCount pwd - 1)
          matcher = (== character pwd)

countValids :: [Password] -> Int
countValids pwds = length (filter isValid pwds)

main = do
    pwds <- getPasswords "input"
    print $ countValids pwds