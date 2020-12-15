import qualified Data.Map as Map

type State = Map.Map Int [Int] -- Map from Number -> Stack of Turn Numbers

pushTurn :: State -> Int -> Int -> State
pushTurn state newNum turnNum =
    case Map.lookup newNum state of
        Nothing -> Map.insert newNum [turnNum] state
        Just stack -> Map.insert newNum (pushStack stack turnNum) state
    where
        pushStack stack turnNum = case length stack == 2 of
                                    True -> drop 1 stack ++ [turnNum]
                                    False -> stack ++ [turnNum]

nextNumber :: State -> Int -> Int -> (Int, State)
nextNumber state startNum turnNum =
    (newNum, pushTurn state newNum turnNum)
    where
     newNum = case Map.lookup startNum state of
                Nothing -> 0
                Just stack -> (last stack) - (head stack)


getNthNumber :: State -> Int -> Int -> Int -> (Int, State)
getNthNumber state target startNum turnNum =
    foldl (\(lastNum, lastState) turnNum -> nextNumber lastState lastNum turnNum) (startNum, state) [turnNum..target]

main = do
    let input = [18, 11, 9, 0, 5, 1]
    let startState = Map.fromList $ zip input (map (\i -> [i]) [1..])
    let finalState = getNthNumber startState 2020 (last input) (length input + 1)
    print $ fst finalState