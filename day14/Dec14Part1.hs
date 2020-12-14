import qualified Data.Bits as Bits
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, optional)
import Text.Parsec.Char (letter, char, digit, space, spaces, string, endOfLine)
import qualified Data.Map as Map

type Mask = [(Int, Maybe Bool)]
data MemoryStmt = MemoryStmt { addr :: Int, value :: Int } deriving (Show)
type Routine = (Mask, [MemoryStmt])
type Program = [Routine]
type Memory = Map.Map Int Int

maskExpr :: Parser Mask
maskExpr = do
    string "mask"
    optional spaces
    char '='
    optional spaces
    maskStr <- many1 (char 'X' <|> char '1' <|> char '0')
    endOfLine
    return $ zip (reverse [0..length maskStr-1]) (map (\c -> case c of
                            'X' -> Nothing
                            '1' -> Just True
                            '0' -> Just False) maskStr)


memExpr :: Parser MemoryStmt
memExpr = do
    string "mem"
    char '['
    addr <- many1 digit
    char ']'
    optional spaces
    char '='
    optional spaces
    value <- many1 digit
    endOfLine
    return $ MemoryStmt { addr = read addr, value = read value }


routineExpr :: Parser Routine
routineExpr = do
    mask <- maskExpr
    memLines <- many1 (try memExpr)
    return $ (mask, memLines)

programExpr :: Parser Program
programExpr = do
    routines <- many1 routineExpr
    return routines

parseProgram :: String -> Program
parseProgram input = case parse programExpr "" input of
    Left err -> error ("Unable to parse program: " ++ show err)
    Right a -> a


getProgram :: FilePath -> IO Program
getProgram path = do
    contents <- readFile path
    return $ parseProgram contents

applyMask :: Mask -> Int -> Int
applyMask [] value = value
applyMask (maskBit:maskBits) value =
    applyMask maskBits newValue
    where
        idx = fst maskBit
        bit = snd maskBit
        newValue = case bit of
                    Nothing -> value
                    Just b -> case b of
                        True -> Bits.setBit value idx
                        False -> Bits.clearBit value idx


setMemory :: Mask -> MemoryStmt -> Memory -> Memory
setMemory mask stmt mem =
    Map.insert (addr stmt) (applyMask mask (value stmt)) mem

execRoutine :: Mask -> [MemoryStmt] -> Memory -> Memory
execRoutine mask [] mem = mem
execRoutine mask (stmt:stmts) mem =
    execRoutine mask stmts (setMemory mask stmt mem)

execProgram :: Program -> Memory -> Memory
execProgram [] mem = mem
execProgram (routine:routines) mem =
    execProgram routines (execRoutine (fst routine) (snd routine) mem)

main = do
    program <- getProgram "input"
    let finalMem = execProgram program Map.empty
    print $ Map.foldl (+) 0 finalMem