import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, optional, chainl1)
import Text.Parsec.Char (letter, char, digit, spaces, string, endOfLine)

data Expr = EAdd Expr Expr | EMul Expr Expr | EInt Int deriving (Show)

symbol :: String -> Parser String
symbol sym = do
    val <- string sym
    optional spaces
    return val

parens :: Parser a -> Parser a
parens m = do
    symbol "("
    n <- m
    symbol ")"
    optional spaces
    return n

int :: Parser Expr
int = do
  n <- many1 digit
  optional spaces
  return (EInt $ read n)

expr :: Parser Expr
expr = factor `chainl1` operation

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = symbol x >> return f

operation :: Parser (Expr -> Expr -> Expr)
operation = (infixOp "+" EAdd) <|> (infixOp "*" EMul)

parseExpr :: String -> Expr
parseExpr input = case parse expr "" input of
                    Left err -> error ("Unable to parse expression: " ++ show err)
                    Right e -> e

eval :: Expr -> Int
eval ex = case ex of
    EAdd a b -> eval a + eval b
    EMul a b -> eval a * eval b
    EInt n   -> n

getExprs :: FilePath -> IO [Expr]
getExprs path = do
    contents <- readFile path
    return $ map parseExpr (lines contents)

main = do
    exprs <- getExprs "input"
    print $ foldl (\accum expr -> accum + eval expr) 0 exprs