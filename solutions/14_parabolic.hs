import Data.List
import System.Environment (getArgs)

data Element = Empty | Round | Cube deriving (Eq, Show)

type Platform = [[Element]]

type Coord = (Int, Int)

data Direction = North | South | East | West deriving (Eq, Show)

at :: Platform -> Coord -> Element
at platform (x, y) = (platform !! y) !! x

width :: Platform -> Int
width platform = length (platform !! 0)

height :: Platform -> Int
height platform = length platform

isLegal :: Platform -> Coord -> Bool
isLegal platform (x, y) = x >= 0 && x < width platform && y >= 0 && y < height platform

set :: Platform -> Coord -> Element -> Platform
set platform (x, y) element = vInserted
    where
        (va, vb) = splitAt y platform
        (ha, hb) = splitAt x (head vb)
        hInserted = ha ++ [element] ++ tail hb
        vInserted = va ++ [hInserted] ++ tail vb

swap :: Platform -> Coord -> Coord -> Platform
swap platform coord1 coord2 = set (set platform coord2 e1) coord1 e2
    where
        e1 = at platform coord1
        e2 = at platform coord2

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let platform = parsePlatform (lines input)
    print (answer1 platform)
    let spins = foldl (\ps c -> ps ++ [spin (last ps)]) [platform] [1..40]
    let loads = map load spins
    let shows = foldl (\ss p -> ss ++ (showPlatform p)) "" spins
    putStr shows
    print (loads)

-- Part 1.

answer1 :: Platform -> Int
answer1 platform = load (roll platform North)

roll :: Platform -> Direction -> Platform
roll platform direction = foldl (\p c -> rollRock p direction c) platform coords
    where
        (w, h) = (width platform, height platform)
        coords = case direction of
            North -> [(x, y) | x <- [0..w-1], y <- [0..h-1]]
            South -> [(x, y) | x <- [0..w-1], y <- reverse [0..h-1]]
            East -> [(x, y) | x <- reverse [0..w-1], y <- [0..h-1]]
            West -> [(x, y) | x <- [0..w-1], y <- [0..h-1]]


rollRock :: Platform -> Direction -> Coord -> Platform
rollRock platform direction coord
    | (at platform coord) /= Round = platform
    | otherwise = swap platform coord (furthest platform direction coord)

furthest :: Platform -> Direction -> Coord -> Coord
furthest platform direction coord
    | not (isLegal platform nextCoord) = coord
    | nextElement /= Empty = coord
    | otherwise = furthest platform direction nextCoord
    where
        nextCoord = next coord direction
        nextElement = at platform nextCoord

next :: Coord -> Direction -> Coord
next (x, y) direction = case direction of
    East -> (x + 1, y)
    West -> (x - 1, y)
    North -> (x, y - 1)
    South -> (x, y + 1)

load :: Platform -> Int
load platform = foldl (+) 0 weights
    where
        (w, h) = (width platform, height platform)
        coords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
        weights = map (weight platform) coords

weight :: Platform -> Coord -> Int
weight platform coord
    | rock /= Round = 0
    | otherwise = dist coord
    where
        rock = at platform coord
        dist (x, y) = (height platform) - y

-- Part 2.

spin :: Platform -> Platform
spin platform = foldl roll platform [North, West, South, East]
--cycle p = roll (roll (roll (roll p North) West) South) East 

-- Parse platform.

parsePlatform :: [String] -> Platform
parsePlatform = map parseRow

parseRow :: String -> [Element]
parseRow line = map (\c -> (parseChar c)) line

parseChar :: Char -> Element
parseChar c
    | c == '.' = Empty
    | c == 'O' = Round
    | c == '#' = Cube

showPlatform :: Platform -> String
showPlatform platform = foldl (\rs r -> rs ++ (showRow r)) "\n" platform
    where
        showRow row = (map asChar row) ++ "\n"


asChar :: Element -> Char
asChar element = case element of
    Empty -> '.'
    Round -> 'O'
    Cube -> '#'