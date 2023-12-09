import Data.List
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let races = parseRaces (lines input)
    print (answer1 races)
    print (answer2 races)

-- Part 1.

answer1 races = foldl (*) 1 (map ways races)

-- Rewritten as a quadratic equation, the push length p required to beat
-- a distance d in a race of length r is: (-p)^2 + rp - d > 0. Use the
-- quadratic equation to solve for the lowest winning push length and
-- highest winning push length, and compute the number of ways to win.
ways :: (Double, Double) -> Int
ways (raceLen, minDist) = highPush - lowPush + 1
    where
        above n = (ceiling n) + (if floor n == ceiling n then 1 else 0)
        below n = (floor n) - (if floor n == ceiling n then 1 else 0)
        det = sqrt (raceLen * raceLen - 4.0 * minDist)
        lowPush = above ((-raceLen + det) / (-2.0))
        highPush = below ((-raceLen - det) / (-2.0))

-- Part 2.

answer2 races = ways (58996469, 478223210191071)

-- Parse races.

parseRaces lines = zip times distances
    where
        (_, timesStr) = split ':' (lines !! 0)
        (_, distancesStr) = split ':' (lines !! 1)
        times = map (\n -> read n :: Double) (words timesStr)
        distances = map (\n -> read n :: Double) (words distancesStr)

split :: Char -> String -> (String, String)
split ch s = (lhs, drop 1 rhs) where (lhs, rhs) = break (== ch) s
