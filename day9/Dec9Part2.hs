import Data.List

data Number = Number { pos :: Int, value :: Int } deriving (Show)
data State = State { idx :: Int, width :: Int, numbers :: [Number] } deriving (Show)

sortedInsert :: Number -> [Number] -> [Number]
sortedInsert x [] = [x]
sortedInsert x (y:ys) =
    if (value x) <= (value y) then
        x:y:ys
    else
        y : sortedInsert x ys

deleteIf :: (a -> Bool) -> [a] -> [a]
deleteIf f list = [ x | x <- list, not (f x) ]

doesSum :: Int -> [Int] -> Bool
doesSum val [] = False
doesSum val list =
    case compare val (head list + last list) of
        EQ -> True
        LT -> doesSum val (take (listSize - 1) list)
        GT -> doesSum val (drop 1 list)
    where
        listSize = length list

validSeq :: Int -> [Int] -> [Int]
validSeq target list =
    if (target == (head (dropWhile (< target) summedList))) then
        case elemIndex target summedList of
            Nothing -> []
            Just idx -> take (idx + 1) list
    else
        []
    where
        summedList = scanl1 (+) list


loadNumbers :: FilePath -> IO [Number]
loadNumbers path = do
    contents <- readFile path
    return $ map (\(idx, line) -> Number { pos = idx, value = read line }) (zip [0..] $ lines contents)


window :: Int -> Int -> [Number] -> [Number]
window width pidx list = deleteIf (\item -> (pos item) <= (pidx - width) || (pos item) > pidx) list

ingestNumber :: State -> Number -> State
ingestNumber state val =
    State { idx = (idx state) + 1, width = width state, numbers = window (width state) (idx state + 1) (sortedInsert val (numbers state)) }


doesSumWindow :: State -> Int -> Bool
doesSumWindow state val = doesSum val (map value (numbers state))

processAllNumbers :: State -> [Number] -> [(Number, Bool)] -> [(Number, Bool)]
processAllNumbers state [] accum = accum
processAllNumbers state (num:list) accum =
    processAllNumbers (ingestNumber state num) list (accum ++ [(num, doesSumWindow state (value num))])


startsValidSeq :: Int -> [Int] -> [Int]
startsValidSeq target [] = []
startsValidSeq target list =
    if not $ null targetSubseq then
        targetSubseq
    else
        startsValidSeq target (drop 1 list)
    where
        targetSubseq = validSeq target list

main = do
    let width = 25
    allNumbers <- loadNumbers "input"
    let state = State { idx = width - 1, width = width, numbers = sortOn (\item -> value item) (window width (width - 1) allNumbers) }
    let invalidNum = head (filter (\(num, valid) -> not valid) $ processAllNumbers state (drop width allNumbers) [])
    print $ invalidNum
    let subSeq = startsValidSeq (value $ fst invalidNum) (map value allNumbers)
    print $ subSeq
    print $ minimum subSeq + maximum subSeq
