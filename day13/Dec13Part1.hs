type BusNotes = (Int, [Int])
type BusArrival = (Int, Int) -- (arrival time, id)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
            where (w, s'') = break p s'

getNotes :: FilePath -> IO BusNotes
getNotes path = do
 contents <- readFile path
 let fileLines = lines contents
 let startTimeLine = fileLines !! 0
 let busTimesLine = fileLines !! 1
 let busTimes = map read (filter (/="x") (wordsWhen (==',') busTimesLine))
 return $ (read startTimeLine, busTimes)


earliestBus :: BusNotes -> BusArrival
earliestBus notes =
    head [(time, id) | time <- [startTime..], id <- times, time `mod` id == 0]
    where
        startTime = fst notes
        times = snd notes

answer :: BusNotes -> BusArrival -> Int
answer notes arrival = 
    (fst arrival - fst notes) * snd arrival

main = do
    notes <- getNotes "input"
    print $ answer notes (earliestBus notes)