import Data.Sort
import Control.Monad (zipWithM)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
            where (w, s'') = break p s'

getArrivals :: FilePath -> IO [(Int, Int)]
getArrivals path = do
    contents <- readFile path
    let fileLines = lines contents
    let busTimesLine = fileLines !! 1
    let busTimes = map (\(idx, str) -> (idx, read str)) (filter (\(idx, str) -> str /= "x") $ zip [0..] (wordsWhen (==',') busTimesLine))
    return $ busTimes

egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b
 
modInv :: Int -> Int -> Either String Int
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for " ++ show a ++ " and " ++ show b
 
chineseRemainder :: [Int] -> [Int] -> Either String Int
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii


earliestValidTime :: [(Int, Int)] -> Int
earliestValidTime arrivals =
   case chineseRemainder as ns of
    Left _ -> 0
    Right a -> a
    where
        modArrivals = map (\(idx, time) -> ((time-idx) `mod` time, time)) arrivals
        as = map fst modArrivals
        ns = map snd modArrivals

main = do
    arrivals <- getArrivals "input"
    print $ earliestValidTime arrivals