import Data.Sort
import qualified Data.Map as Map


loadNumbers :: FilePath -> IO [Int]
loadNumbers path = do
    contents <- readFile path
    return $ sort (map read $ lines contents)


traverseList :: [Int] -> [Int]
traverseList list =
    foldr (\i accum -> sum [accum !! (j-i-1) | j <- [i+1..i+3], j < length list, (list !! j) - (list !! i) <= 3] : accum) [1] [0..(length list)-2]

main = do
    numbers <- loadNumbers "input"
    let allNums = [0] ++ numbers ++ [maximum numbers + 3]
    print $ head (traverseList allNums)