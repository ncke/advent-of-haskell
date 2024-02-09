import Data.List
import qualified Data.Set as Set
import System.Environment (getArgs)

-- Garden.

type GardenMap = [String]

size :: GardenMap -> (Int, Int)
size garden_map = (length (garden_map !! 0), length garden_map)

type Coord = (Int, Int)

is_legal :: GardenMap -> Coord -> Bool
is_legal garden_map (x, y) = x >= 0 && x < x_sz && y >= 0 && y < y_sz
    where
        (x_sz, y_sz) = size garden_map

as_finite :: GardenMap -> Coord -> Coord
as_finite garden_map (x, y) = (x `mod` x_sz, y `mod` y_sz)
    where
        (x_sz, y_sz) = size garden_map

data Square = Rock | Garden | Start deriving (Eq, Show)

at_coord :: GardenMap -> Coord -> Square
at_coord garden_map (x, y)
    | sq_char == '.' = Garden
    | sq_char == 'S' = Start
    | sq_char == '#' = Rock
    where
        sq_char = (garden_map !! y) !! x

is_traversable :: GardenMap -> Coord -> Bool
is_traversable garden_map coord = (at_coord garden_map coord) /= Rock

find_start :: GardenMap -> Coord
find_start garden_map = starts !! 0
    where
        (x_sz, y_sz) = size garden_map
        coords = [(x, y) | x <- [0..x_sz-1], y <- [0..y_sz-1]]
        starts = filter (\c -> Start == at_coord garden_map c) coords

-- Grid.

type Grid t = [[t]]

get :: Grid t -> Coord -> t
get g (x, y) = (g !! y) !! x

set :: Grid t -> Coord -> t -> Grid t
set g (x, y) v = changed_grid
    where
        rows_before = take y g
        rows_after = drop (y + 1) g
        affected_row = g !! y
        cols_before = take x affected_row
        cols_after = drop (x + 1) affected_row
        changed_row = cols_before ++ [v] ++ cols_after
        changed_grid = rows_before ++ [changed_row] ++ rows_after

empty :: (Int, Int) -> t -> Grid t
empty (sz_x, sz_y) v = take sz_y (repeat (take sz_x (repeat v)))

grid_size :: Grid t -> (Int, Int)
grid_size g = (length (g !! 0), length g)

all_coords :: Grid t -> [Coord]
all_coords g = [(x, y) | x <- [0..sz_x-1], y <- [0..sz_y-1]] where (sz_x, sz_y) = grid_size g

-- Main.

main :: IO()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let garden_map = lines input
    let start = find_start garden_map

    let a1 = answer1 garden_map (Set.fromList [start]) 6 
    print(a1)

    print(start)

    let a2 = answer2 garden_map start 6
    print(a2)
    print(total_visitors a2)

-- Answer 1.

answer1 :: GardenMap -> Set.Set Coord -> Int -> Int
answer1 garden_map locns 0 = length locns
answer1 garden_map locns step_count = 
    answer1 garden_map (finite_moves garden_map locns) (step_count - 1)

finite_moves :: GardenMap -> Set.Set Coord -> Set.Set Coord
finite_moves garden_map locns = collate
    where
        next_locns = map (finite_move garden_map) (Set.toList locns)
        collate = foldl (Set.union) Set.empty next_locns

finite_move :: GardenMap -> Coord -> Set.Set Coord
finite_move garden_map (x, y) = Set.fromList traversables
    where
        displacements = [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]
        legals = filter (is_legal garden_map) displacements
        traversables = filter (is_traversable garden_map) legals

-- Answer 2.

--answer2 :: GardenMap -> Set.Set Coord -> Int -> Int
answer2 garden_map start step_count = next neighbours traversables initial step_count
    where
        neighbours = neighbour_grid garden_map
        traversables = traversable_grid garden_map
        initial = initial_visitors garden_map start

next _ _ visitors 0 = visitors --total_visitors visitors
next neighbours traversables visitors step_count = next neighbours traversables (next_visitors neighbours traversables visitors) (step_count - 1) 

neighbour_grid :: GardenMap -> Grid [Coord]
neighbour_grid garden_map = neigh_grid
    where
        (sz_x, sz_y) = size garden_map
        displacements (x, y) = [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]
        traversables coord = filter (\c -> is_traversable garden_map (as_finite garden_map c)) (displacements coord)  
        neighbours coord = map (as_finite garden_map) (traversables coord)
        coords = [(x, y) | x <- [0..sz_x-1], y <- [0..sz_y-1]]
        empty_grid = empty (sz_x, sz_y) []
        neigh_grid = foldl (\g c -> set g c (neighbours c)) empty_grid coords

traversable_grid :: GardenMap -> Grid Bool
traversable_grid garden_map = trav_grid
    where
        empty_grid = empty (size garden_map) False
        trav_grid = foldl (\g c -> set g c (is_traversable garden_map c)) empty_grid (all_coords empty_grid)

initial_visitors :: GardenMap -> Coord -> Grid Int
initial_visitors garden_map start = set (empty (size garden_map) 0) start 1

next_visitors :: Grid [Coord] -> Grid Bool -> Grid Int -> Grid Int
next_visitors neighbours traversables visitors = next_grid
    where
        empty_grid = empty (grid_size visitors) 0
        neighs c = map (get visitors) (get neighbours c)
        sum c = foldl (+) 0 (neighs c)
        is_traversable c = get traversables c
        next_grid = foldl (\g c -> if is_traversable c then set g c (sum c) else g) empty_grid (all_coords visitors)

total_visitors :: Grid Int -> Int
total_visitors visitors = foldl (\t c -> t + get visitors c) 0 (all_coords visitors) 
