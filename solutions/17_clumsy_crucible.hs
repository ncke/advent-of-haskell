import Data.List
import qualified Data.Set as Set
import System.Environment (getArgs)

type Grid = [[Int]]

dimensions :: Grid -> (Int, Int)
dimensions grid = (length (grid !! 0), length grid)

type Coord = (Int, Int)

isLegal :: Grid -> Coord -> Bool
isLegal grid (x, y) = 
    x >= 0 && x <= w && y >= 0 && y <= h where (w, h) = dimensions grid

at :: Grid -> Coord -> Int
at grid (x, y) = (grid !! y) !! x

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let grid = parseGrid (lines input)
    print (answer1 grid)
    print (answer2 grid)

-- Part 1.

answer1 grid = 42

-- Part 2.

answer2 grid  = 42

-- Parse grid.

parseGrid :: [String] -> Grid
parseGrid lines = map parseLine lines

parseLine :: String -> [Int]
parseLine line = map (\c -> read [c] :: Int) line
