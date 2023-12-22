import Data.List
import qualified Data.Set as Set
import System.Environment (getArgs)

type Coord = (Int, Int)

data Element = Empty | UpMirror | DownMirror | HSplitter | VSplitter
    deriving (Eq, Show)

type Grid = [[Element]]

at :: Grid -> Coord -> Element
at grid (x, y) = (grid !! y) !! x

isLegal :: Grid -> Coord -> Bool
isLegal grid (x, y) = x >= 0 && x < width grid && y >= 0 && y < height grid

width :: Grid -> Int
width grid = length (grid !! 0)

height :: Grid -> Int
height grid = length grid

data Direction = North | South | East | West deriving (Eq, Show)

type Move = (Coord, Direction)

main :: IO ()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let grid = parseGrid (lines input)
    print (answer1 grid)
    print (answer2 grid)

-- Part 1.

answer1 grid = length (Set.fromList coords)
    where
        visiteds = beam grid [] [((0, 0), East)]
        coords = map fst visiteds

beam :: Grid -> [Move] -> [Move] -> [Move]
beam _ acc [] = acc
beam grid acc ((coord, direction) : remaining)
    | isLegal grid coord && unvisited = beam grid accumulated (remaining ++ afters)
    | otherwise = beam grid acc remaining
    where
        accumulated = (coord, direction) : acc
        unvisited = Nothing == find (==(coord, direction)) acc
        dirs = bounces direction (at grid coord)
        afters = zip (map (next coord) dirs) dirs

next :: Coord -> Direction -> Coord
next (x, y) direction = case direction of
    East -> (x + 1, y)
    West -> (x - 1, y)
    North -> (x, y - 1)
    South -> (x, y + 1)

bounces :: Direction -> Element -> [Direction]
bounces input element
    | element == UpMirror && input == East = [North]
    | element == UpMirror && input == West = [South]
    | element == UpMirror && input == North = [East]
    | element == UpMirror && input == South = [West]
    | element == DownMirror && input == East = [South]
    | element == DownMirror && input == West = [North]
    | element == DownMirror && input == South = [East]
    | element == DownMirror && input == North = [West]
    | element == VSplitter && (input == East || input == West) = [North, South]
    | element == HSplitter && (input == North || input == South) = [East, West]
    | otherwise = [input]

-- Part 2.

answer2 grid = maximum (map (from grid) (starts grid)) 

from :: Grid -> Move -> Int
from grid move = length (Set.fromList coords)
    where
        visiteds = beam grid [] [move]
        coords = map fst visiteds

starts :: Grid -> [Move]
starts grid = top ++ bot ++ lef ++ rig
    where
        top = [((x, 0), South) | x <- [0..width grid]]
        bot = [((x, height grid - 1), North) | x <- [0..width grid]]
        lef = [((0, y), East) | y <- [0..height grid]]
        rig = [((width grid - 1, y), West) | y <- [0..height grid]]

-- Parse grid.

parseGrid :: [String ] -> Grid
parseGrid = map parseRow

parseRow :: String -> [Element]
parseRow line = map (\c -> (parseChar c)) line

parseChar :: Char -> Element
parseChar c
    | c == '.' = Empty
    | c == '/' = UpMirror
    | c == '\\' = DownMirror
    | c == '|' = VSplitter
    | c == '-' = HSplitter
