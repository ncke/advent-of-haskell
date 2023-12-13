import Data.List
import qualified Data.Set as Set
import System.Environment (getArgs)

type Coord = (Int, Int)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let galaxies = parseImage (lines input)
    let size = sizeImage (lines input)

    -- add 1 extra to each empty row or columm.
    print (answer 1 size galaxies)

    -- add 999,999 extra to each empty row or column.
    print (answer 999999 size galaxies)

-- Cosmic expansion.

answer factor (xSize, ySize) galaxies = foldl (+) 0 (map dist pairs)
    where
        extended = yExtend factor ySize (xExtend factor xSize galaxies)
        pairs = [(p, q) | p <- [0..length extended-1], q <- [0..p-1]] 
        dist (p, q) = distance (extended !! p) (extended !! q)
        
distance :: Coord -> Coord -> Int
distance (px, py) (qx, qy) = abs (qy - py) + abs (qx - px)

xExtend :: Int -> Int -> [Coord] -> [Coord]
xExtend factor xSize galaxies = map (\(x, y) -> (x + factor * offset x, y)) galaxies
    where
        xs = Set.fromList (map (\(x, y) -> x) galaxies)
        all = Set.fromList [0..xSize-1]
        empty = Set.difference all xs
        offset x = length (filter (<x) (Set.toList empty))

yExtend :: Int -> Int -> [Coord] -> [Coord]
yExtend factor ySize galaxies = map (\(x, y) -> (x, y + factor * offset y)) galaxies
    where
        ys = Set.fromList (map (\(x, y) -> y) galaxies)
        all = Set.fromList [0..ySize-1]
        empty = Set.difference all ys
        offset y = length (filter (<y) (Set.toList empty))

-- Parse images.

sizeImage :: [String] -> (Int, Int)
sizeImage lines = (length (lines !! 0), length lines)

parseImage :: [String] -> [Coord]
parseImage lines = concatMap (\y -> parseLine (lines !! y) y) [0..length lines-1]

parseLine :: String -> Int -> [Coord]
parseLine line y = repns line
    where
        repn acc x = if line !! x == '#' then (x, y) : acc else acc
        repns line = foldl (\acc x -> repn acc x) [] [0..length line-1]
