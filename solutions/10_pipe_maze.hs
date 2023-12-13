import Data.List
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import System.Environment (getArgs)

-- Maze.

data Direction = North | South | East | West deriving (Eq, Show)

type Square = [Direction]

type Maze = [[[Direction]]]

type Coord = (Int, Int)

type Path = [(Int, Int)]

has :: Square -> Direction -> Bool
has square direction = Nothing /= find (== direction) square

width :: Maze -> Int
width maze = length (maze !! 0)

height :: Maze -> Int
height maze = length maze

legal :: Maze -> Coord -> Bool
legal maze (x, y) = x >= 0 && x < width maze && y >= 0 && y < height maze 

at :: Maze -> Coord -> [Direction]
at maze (x, y) = if legal maze (x, y) then (maze !! y) !! x else []

-- Solve.

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let (start, maze) = parseMaze (lines input)
    let loop = findLoop maze start [[start]]
    print (answer1 loop)
    putStrLn (answer2 maze loop)
    print (answer3 maze loop)
    
-- Part 1.

answer1 :: Path -> Int
answer1 loop = (length loop) `div` 2

findLoop :: Maze -> Coord -> [Path] -> Path
findLoop maze start paths =
    if null connected then findLoop maze start nextPaths else head connected
    where
        nextPaths = concatMap (advancePath maze start) paths
        connected = filter hasCircuit nextPaths

hasCircuit :: Path -> Bool
hasCircuit path = head path == last path && length path > 1

advancePath :: Maze -> Coord -> [Coord] -> [[Coord]]
advancePath maze start path = map (\n -> n : path) next 
    where
        cursor = head path
        candidates = map (applyMove cursor) (at maze cursor)
        isPrevious coord = [coord] == drop 1 (take 2 path)
        noBacktracks = filter (not . isPrevious) candidates
        next = filter (unvisited path) noBacktracks

unvisited :: Path -> Coord -> Bool
unvisited path coord = Nothing == find (== coord) (init path)

applyMove :: Coord -> Direction -> Coord
applyMove (x, y) direction 
    | direction == North = (x, y - 1)
    | direction == South = (x, y + 1)
    | direction == East = (x + 1, y)
    | direction == West = (x - 1, y)

-- Part 2.

answer2 maze loop = display (2 * width maze, 2 * height maze) (Set.fromList (extend maze loop))

answer3 maze loop = finalCoords
    where 
        extendedLoop = Set.fromList (extend maze loop)
        (w, h) = (2 * width maze, 2 * height maze)
        allCoords = [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]
        insideCoords = filter (\coord -> isInside (w, h) extendedLoop (Set.fromList [coord]) coord) allCoords
        isOriginal (x, y) = (x `mod` 2 == 0) && (y `mod` 2 == 0)
        finalCoords = filter (\coord -> isOriginal coord && Set.member coord extendedLoop == False) insideCoords    

extend :: Maze -> [Coord] -> [Coord]
extend maze loop = concatMap (\coord -> extendSquare coord (at maze coord)) loop 

extendSquare :: Coord -> Square -> [Coord]
extendSquare (x, y) sq = 
    [(2 * x, 2 * y)] 
    ++ (if has sq East then [(2 * x + 1, 2 * y)] else [])
    ++ (if has sq South then [(2 * x, 2 * y + 1)] else [])

isInside :: (Int, Int) -> Set.Set Coord -> Set.Set Coord -> Coord -> Bool
isInside (w, h) loop visited (x, y) =
    if hasReachedEdge then False else hasReachedLoop || recurse
    where
        hasReachedEdge = x == 0 || x == w || y == 0 || y == h
        hasReachedLoop = Set.member (x, y) loop
        updatedVisited = Set.insert (x, y) visited
        isUnvisited coord = Set.member coord visited == False
        visit coord = if isUnvisited coord then isInside (w, h) loop updatedVisited coord else True
        recurse = 
            visit (x - 1, y) && visit (x + 1, y) && visit (x, y - 1) && visit (x, y + 1)

display (w, h) loop = concatMap (\r -> r ++ "\n") rows 
    where 
        rows = map displayRow [0..h-1]
        repn coord = if Set.member coord loop then "*" else "."
        displayRow y = concatMap (\x -> repn (x, y)) [0..w-1]

-- Parse maze.

parseMaze :: [String] -> (Coord, Maze)
parseMaze lines = (start, inferStart (map parseLine lines) start)
    where start = findStart lines

parseLine line = map parseDirections line

parseDirections tile 
    | tile == 'S' = []
    | tile == '.' = []
    | tile == '|' = [North, South]
    | tile == '-' = [East, West]
    | tile == 'L' = [North, East]
    | tile == 'J' = [North, West]
    | tile == '7' = [South, West]
    | tile == 'F' = [South, East]

inferStart :: Maze -> Coord -> Maze
inferStart maze (x, y) = replace (x, y) (concatMap infer offsets)
    where 
        offsets = [ ((0, 1), North, South), 
                    ((0, -1), South, North),
                    ((-1, 0), East, West),  
                    ((1, 0), West, East) ]
        infer ((ox, oy), opposite, inference) = 
            if has (at maze (x + ox, y + oy)) opposite then [inference] else []
        set value idx list = take idx list ++ [value] ++ drop (idx + 1) list
        replace (x, y) directions = set (set directions x (maze !! y)) y maze       

findStart :: [String] -> Coord
findStart lines = fromJust (find isStart coords)
    where
        (w, h) = (length (lines !! 0), length lines)
        coords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
        isStart (x, y) = 'S' == (lines !! y) !! x
