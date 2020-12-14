import qualified Data.Bits as Bits
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, optional)
import Text.Parsec.Char (letter, char, digit, space, spaces, string, endOfLine)
import Data.Maybe (catMaybes)
import Control.Monad (replicateM)
import qualified Data.Map as Map


type FloatingMask = [(Int, Maybe Bool)]
type Mask = [(Int, Bool)]
data MemoryStmt = MemoryStmt { addr :: Int, value :: Int } deriving (Show)
type Routine = (FloatingMask, [MemoryStmt])
type Program = [Routine]
type Memory = Map.Map Int Int

maskExpr :: Parser FloatingMask
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

toBin :: Int -> [Bool]
toBin 0 = [False]
toBin n
    | n `mod` 2 == 1 = toBin (n `div` 2) ++ [True]
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [False]

decimal :: [Bool] -> Int
decimal [] = 0
decimal (x:xs) = case x of
                True -> 2^(length xs) + decimal xs
                False -> decimal xs

mask :: Int -> Mask
mask num =
    zip (reverse [0..35]) ([ False | i <- [1..padding] ] ++ binRep)
    where
        binRep = toBin num
        padding = 36 - length binRep

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceAt (n-1) newVal xs


catFloatingMasks :: [FloatingMask] -> [Mask]
catFloatingMasks inputMasks =
    map unrollMask inputMasks
    where
        unrollMask inputMask = catMaybes $ map (\(idx, b) -> case b of
                                                        Nothing -> Nothing
                                                        Just a -> Just (idx, a)) inputMask

replaceAll :: FloatingMask -> FloatingMask -> FloatingMask
replaceAll inputMask [] = inputMask
replaceAll inputMask (bit:bits) =
    replaceAll newMask bits
    where
        newMask = replaceAt (length inputMask - (fst bit) - 1) bit inputMask


allMasks :: FloatingMask -> Int -> [Mask]
allMasks inputMask addr =
    catFloatingMasks [ replaceAll appliedMask fv | fv <- floatingValues ]
    where
        appliedMask = applyMask inputMask (mask addr)
        floatingIdxs = map fst (filter (\(_, b) -> case b of
                                                Nothing -> True
                                                Just _ -> False) appliedMask)
        floatingValues = map (zip floatingIdxs) (replicateM (length floatingIdxs) [Just True, Just False])


applyMask :: FloatingMask -> Mask -> FloatingMask
applyMask maskA maskB = [ case bitA of
                            (idx, Nothing) -> (idx, Nothing)
                            (idx, Just val) -> case val of
                                True -> (idx, Just True)
                                False -> (idx, Just (snd bitB)) | idx <- [0..35], (bitAidx, bitA) <- zip [0..] maskA, (bitBidx, bitB) <- zip [0..] maskB, bitAidx == idx, bitBidx == idx ]


setMemory :: [MemoryStmt] -> Memory -> Memory
setMemory stmts mem =
    foldl (\accum stmt -> Map.insert (addr stmt) (value stmt) accum) mem stmts

execRoutine :: FloatingMask -> [MemoryStmt] -> Memory -> Memory
execRoutine mask [] mem = mem
execRoutine mask (stmt:stmts) mem =
    execRoutine mask stmts (setMemory (allStmts stmt) mem)
    where
        allStmts stmt = map (\m -> MemoryStmt { addr = decimal (map snd m), value = value stmt }) (allMasks mask (addr stmt))

execProgram :: Program -> Memory -> Memory
execProgram [] mem = mem
execProgram (routine:routines) mem =
    execProgram routines (execRoutine (fst routine) (snd routine) mem)


main = do
    program <- getProgram "input"
    let finalMem = execProgram program Map.empty
    print $ Map.foldl (+) 0 finalMem