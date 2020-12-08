import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, eof, sepBy1, manyTill)
import Text.Parsec.Char (anyChar, letter, char, space, digit, endOfLine)
import qualified Data.Map as Map

data Instruction = Acc Int | Jmp Int | Nop deriving (Show)
data ExecState = ExecState { pc :: Int, accum :: Int, loopMap :: Map.Map Int Int } deriving (Show)

emptyLoopMap :: Int -> Map.Map Int Int
emptyLoopMap size =
    Map.fromList [(i, 0) | i <- [0..size]]

signToInt :: Char -> Int
signToInt sign
    | sign == '+' = 1
    | sign == '-' = -1

accExpr :: Parser Instruction
accExpr = do
    string "acc"
    space
    sign <- char '+' <|> char '-'
    number <- many1 digit
    return $ Acc ((read number) * signToInt sign)

jmpExpr :: Parser Instruction
jmpExpr = do
    string "jmp"
    space
    sign <- char '+' <|> char '-'
    number <- many1 digit
    return $ Jmp ((read number) * signToInt sign)

nopExpr :: Parser Instruction
nopExpr = do
    string "nop"
    return Nop

instructionExpr :: Parser Instruction
instructionExpr = accExpr <|> jmpExpr <|> nopExpr


parseLine :: String -> Instruction
parseLine line = case parse instructionExpr "" line of
    Left _ -> error "Invalid instruction."
    Right a -> a

loadInstructions :: FilePath -> IO [Instruction]
loadInstructions path = do
    contents <- readFile path
    return $ map parseLine (lines contents)

eval :: Instruction -> ExecState -> ExecState
eval instr state =
    case instr of
        Nop        -> ExecState { pc = pc state + 1, accum = accum state, loopMap = Map.adjust (+1) (pc state) (loopMap state) }
        Jmp offset -> ExecState { pc = pc state + offset, accum = accum state, loopMap = Map.adjust (+1) (pc state) (loopMap state) }
        Acc offset -> ExecState { pc = pc state + 1, accum = accum state + offset, loopMap = Map.adjust (+1) (pc state) (loopMap state) }

process :: [Instruction] -> ExecState -> ExecState
process instrs state =
    case Map.lookup (pc state) (loopMap state) of
        Just count -> if count > 0 then state else process instrs newState
        Nothing -> state
    where
        instr = instrs !! (pc state)
        newState = eval instr state

main = do
    instructions <- loadInstructions "input"
    print $ accum (process instructions ExecState { pc = 0, accum = 0, loopMap = emptyLoopMap (length instructions) })