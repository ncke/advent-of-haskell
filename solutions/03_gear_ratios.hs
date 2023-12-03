import Data.Char (ord)
import Data.List
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let answer = gearRatios (lines input)
    print answer

gearRatios :: [String] -> Int
gearRatios schematic = answer2 schematic

-- Part 1.

answer1 :: [String] -> Int
answer1 schematic = foldl (+) 0 (getAdjacentNumbers schematic)

getAdjacentNumbers :: [String] -> [Int]
getAdjacentNumbers strs = map (\(s,n) -> read s :: Int) adjacents
    where
        nums = getNumbers strs [] 0
        hasSymbol str = (find isSymbol str) /= Nothing
        adjacents = filter (\(s,n) -> hasSymbol n) nums

getNumbers :: [[Char]] -> [(String, String)] -> Int -> [(String, String)]
getNumbers strs nums y =
    if y == length strs then nums
    else getNumbers strs (nums ++ getNumbersLine strs [] 0 y) (y + 1)

getNumbersLine :: [[Char]] -> [(String, String)] -> Int -> Int -> [(String, String)]
getNumbersLine strs nums x y =
    if x == length str 
        then filtered nums
        else if isDigit ch
            then getNumbersLine strs (addNeighboursToFirst (addCharToFirst nums ch) neighs) (x + 1) y
            else getNumbersLine strs (([],[]) : nums) (x + 1) y
    where
        str = strs !! y
        ch = str !! x
        neighs = neighbours' strs x y [] neighbourOffsets
        filtered nums = filter (\(s,n) -> s /= "") nums

addCharToFirst :: [(String, String)] -> Char -> [(String, String)]
addCharToFirst [] ch = [([ch], [])]
addCharToFirst ((hs, hn) : remainder) ch = (hs ++ [ch], hn) : remainder

addNeighboursToFirst :: [(String, String)] -> [Char] -> [(String, String)]
addNeighboursToFirst [] chs = [([], chs)]
addNeighboursToFirst ((hs, hn) : remainder) ch = (hs, hn ++ ch) : remainder

at :: [String] -> Int -> Int -> Maybe Char
at strs x y = if isLegal then Just ((strs !! y) !! x) else Nothing
    where
        width = length (strs !! 0)
        height = length strs
        isLegal = x >= 0 && x < width && y >= 0 && y < height

neighbourOffsets = [(-1,1), (0,1), (1,1), (-1,0), (1,0), (-1,-1), (0,-1), (1,-1)]

neighbours' :: [String] -> Int -> Int -> [Char] -> [(Int, Int)] -> [Char]
neighbours' strs x y neighs [] = neighs
neighbours' strs x y neighs ((xoff, yoff) : offsets) = case at strs (x + xoff) (y + yoff) of
    Just ch -> neighbours' strs x y (ch : neighs) offsets
    Nothing -> neighbours' strs x y neighs offsets

-- Part 2.

answer2 schematic =
    foldl (+) 0 (map (ratio schematic) gears)
    where
        gears = gearCoords schematic

ratio schematic gear =
    if length adjs /= 2 then 0 else (nums !! 0) * (nums !! 1)
    where
        adjs = adjNumbers schematic gear
        nums = map (numberAt schematic) adjs

numberAt schematic (x, y) =
    read str :: Int
    where
        len = numberLength schematic (x, y)
        row = schematic !! y
        str = take len (drop x row)

adjNumbers schematic (gx, gy) =
    filter (areAnyInsideNumber schematic ns) numbers 
    where
        numbers = numberCoords schematic
        ns = neighbours schematic (gx, gy)

gearCoords schematic = filter (isGear . at) (allCoords schematic) 
    where
        at (x, y) = (schematic !! y) !! x
        isGear ch = ch == '*'

digitCoords schematic = filter (isDigit . at) (allCoords schematic)
    where
        at (x, y) = (schematic !! y) !! x

numberCoords schematic =
    filter isFirst coords
    where
        coords = digitCoords schematic
        isFirst (x, y) = (find (\(i,j) -> j == y && i == x-1) coords) == Nothing

isInsideNumber schematic (nx, ny) (x, y) =
    ny == y && x >= nx && x <= xLast
    where
        xLast = nx - 1 + numberLength schematic (nx, ny)

areAnyInsideNumber schematic coords (nx, ny) =
    find (isInsideNumber schematic (nx, ny)) coords /= Nothing

numberLength schematic (x, y) =
    if x < width - 1 && this > 0 then this + next else this
    where
        this = if isDigit((schematic !! y) !! x) then 1 else 0
        next = numberLength schematic (x + 1, y)
        width = length (schematic !! 0)

neighbours schematic (x, y) =
    filter isLegal candidates
    where
        candidates = map (\(ox,oy) -> (x + ox, y + oy)) neighbourOffsets
        width = length (schematic !! 0)
        height = length schematic
        isLegal (x, y) = x >= 0 && x < width && y >= 0 && y <= height

allCoords schematic =
    [(x,y) | x <- [0..width-1], y <- [0..height-1]]
    where
        width = length (schematic !! 0)
        height = length schematic


        

-- Schematic parsing.

isDigit :: Char -> Bool
isDigit ch = ord ch >= ord('0') && ord ch <= ord('9')

isSymbol :: Char -> Bool
isSymbol ch = ch /= '.' && not (isDigit ch) 
