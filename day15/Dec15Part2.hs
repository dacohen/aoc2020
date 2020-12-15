import qualified Data.Map as Map

type State = Map.Map Int [Int] -- Map from Number -> Stack of Turn Numbers

pushTurn :: State -> Int -> Int -> State
pushTurn state newNum turnNum =
    Map.alter (\mapVal -> case mapVal of
                            Nothing -> Just [turnNum]
                            Just stack -> case length stack == 2 of
                                            True -> Just $ drop 1 stack ++ [turnNum]
                                            False -> Just $ stack ++ [turnNum]) newNum state

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
    let finalState = getNthNumber startState 30000000 (last input) (length input + 1)
    print $ fst finalState