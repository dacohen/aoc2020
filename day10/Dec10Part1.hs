import Data.Sort
import qualified Data.Map as Map

loadNumbers :: FilePath -> IO [Int]
loadNumbers path = do
    contents <- readFile path
    return $ sort (map read $ lines contents)


diff :: [Int] -> [Int]
diff [] = []
diff (x:[]) = []
diff (x:y:list) =
    [y - x] ++ diff ([y] ++ list)

histogram :: [Int] -> Map.Map Int Int
histogram list =
    Map.fromList [(num, count num list) | num <- list]
    where
        count x = length . filter (==x)

main = do
    numbers <- loadNumbers "input"
    let hist = histogram (diff ([0] ++ numbers ++ [maximum numbers + 3]))
    print hist
    print $ (hist Map.! 1) * (hist Map.! 3)