import Data.List
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let readings = parseReadings (lines input)
    print (answer1 readings)
    print (answer2 readings)

-- Part 1.

answer1 readings = foldl (+) 0 (map predict readings)

-- We find the deltas between each element in the series. If this
-- is a constant (the same difference for all elements) then we can
-- predict the next by adding this constant to the last. Otherwise,
-- we recurse by predicting over the deltas.
predict series = next
    where
        deltas = differences series
        next = case allConstant deltas of
            Just n -> (last series) + n
            Nothing -> (last series) + predict deltas

-- Given a list of integers, returns a list of the difference,
-- 'delta', between successive elements.
differences :: [Int] -> [Int]
differences series = take (length deltas - 1) deltas
    where
        diffs [_] = []
        diffs (e1 : e2 : es) = (e2 - e1) : diffs (e2 : es)
        deltas = diffs (series ++ [0])
        
allConstant ns = if allSame then Just first else Nothing
    where 
        first = head ns
        allSame = Nothing == find (/= first) ns

-- Part 2.

answer2 readings = foldl (+) 0 (map backwards readings)

-- We can predict the previous element by reversing the series and
-- then applying the existing technique.
backwards series = predict (reverse series)

-- Parse readings.

parseReadings lns = map convert lns
    where convert ln = map (\n -> read n :: Int) (words ln)
