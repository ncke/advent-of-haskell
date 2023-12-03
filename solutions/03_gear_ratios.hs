import Data.Char (ord)
import Data.List
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let schematic = lines input
    print (answer1 schematic)
    print (answer2 schematic)

-- Part 1.

-- Find the coordinates of all the numbers and symbols. Determine the
-- list of interior coordinates for each number, and determine the
-- neighbourhood of coordinates bordering the interior. Qualifying
-- coordinates are for those numbers with a neighbourhood that contains
-- a symbol. Extract the number at each qualifying coordinate, and sum.
answer1 schematic =
    foldl (+) 0 (map (numberAt schematic) qualifyingCoords)
    where
        symbols = symbolCoords schematic
        numbers = numberCoords schematic
        (w, h) = size schematic
        len (nx, ny) = numberLength schematic (nx, ny)
        interior (nx, ny) = [(i, ny) | i <- [nx..nx - 1 + len (nx, ny)]]
        neighbourhood (nx, ny) = [c | c <- concatMap (neighbours (w, h)) (interior (nx, ny))]
        at (x, y) = (schematic !! y) !! x
        containsSymbol coords = (find (isSymbol . at) coords) /= Nothing
        qualifyingCoords = filter (containsSymbol . neighbourhood) numbers

-- Part 2.

-- Find the coordinates of all the gears. For each, find the coordinates
-- of adjacent numbers and extract those numbers from the schematic.
-- Compute the product when there are two adjacent numbers, and sum.
answer2 schematic =
    foldl (+) 0 (map (ratio . extracts) gears)
    where
        nums = numberCoords schematic
        gears = gearCoords schematic
        adjacents gear = adjacentNumbers schematic nums gear
        extracts gear = map (numberAt schematic) (adjacents gear)
        ratio acts = if length acts /= 2 then 0 else foldl (*) 1 acts

-- Schematic parsing.

-- Returns an Int representation of the number at coordinate (x, y).
numberAt schematic (x, y) =
    read str :: Int
    where
        len = numberLength schematic (x, y)
        row = schematic !! y
        str = take len (drop x row)

-- Returns a list containing the coordinates of any number that extends
-- into a square bordering the coordinate (x, y).
adjacentNumbers schematic nums (x, y) =
    filter (areAnyInsideNumber schematic ns) nums 
    where
        (w, h) = size schematic
        ns = neighbours (w, h) (x, y)

-- Returns a list of the coordinates of all gears.
gearCoords schematic = filter (isGear . at) (allCoords schematic) 
    where
        at (x, y) = (schematic !! y) !! x

-- Returns a list of the coordinates of the first digit of all numbers.
numberCoords schematic = filter isFirst coords
    where
        at (x, y) = (schematic !! y) !! x
        coords = filter (isDigit . at) (allCoords schematic)
        isFirst (x, y) = (find (\(i,j) -> j == y && i == x-1) coords) == Nothing

-- Returns a list of the coordinates of all symbols.
symbolCoords schematic = filter (isSymbol . at) (allCoords schematic)
    where
        at (x, y) = (schematic !! y) !! x

-- Returns true if any coordinate from the coords list is inside the
-- extent of the number beginning at coordinate (nx, ny), otherwise false.
areAnyInsideNumber schematic coords (nx, ny) =
    find (isInside) coords /= Nothing
    where
        len = numberLength schematic (nx, ny)
        isInside (x, y) = ny == y && x >= nx && x < nx + len

-- Returns a count of the number of digits in the number beginning at
-- coordinate (x, y).
numberLength schematic (x, y) =
    if x < width - 1 && this > 0 then this + next else this
    where
        this = if isDigit((schematic !! y) !! x) then 1 else 0
        next = numberLength schematic (x + 1, y)
        width = length (schematic !! 0)

-- Given a schematic size of (w, h), returns a list of all coordinates
-- that border coordinate (x, y) and are inside the schematic's bounds.
neighbours (w, h) (x, y) =
    filter isLegal candidates
    where
        offsets = [(-1,1),(0,1),(1,1),(-1,0),(1,0),(-1,-1),(0,-1),(1,-1)]
        candidates = map (\(ox,oy) -> (x + ox, y + oy)) offsets
        isLegal (x, y) = x >= 0 && x < w && y >= 0 && y < h

-- Returns (w, h), the width and height of the schematic.
size schematic = (length (schematic !! 0), length schematic)

-- Returns a list of all coordinates within the schematic.
allCoords schematic =
    [(x, y) | x <- [0..w-1], y <- [0..h-1]] where (w, h) = size schematic

-- True if ch is a digit, false otherwise.
isDigit ch = ord ch >= ord('0') && ord ch <= ord('9')

-- True if ch is a symbol, false otherwise.
isSymbol ch = ch /= '.' && not (isDigit ch)

-- True if ch is a gear, false otherwise.
isGear ch = ch == '*'
