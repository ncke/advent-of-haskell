import Data.List
import qualified Data.Map as Map
import System.Environment (getArgs)

-- Brick.

type Coord = (Int, Int, Int)

type Brick = (Coord, Coord)

top :: Brick -> Int
top ((_, _, _), (_, _, z2)) = z2

bottom :: Brick -> Int
bottom ((_, _, z1), (_, _, _)) = z1

-- Answers.

main :: IO()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let bricks = parseBricks(lines input)
    let rested = descend_all bricks
    let counts = map (descend_count rested) rested

    -- Find how many bricks can be removed without further movement.
    let answer1 = length (filter (0 ==) counts)
    print(answer1)

    -- Sum the number of removals caused by the removal of each brick.
    let answer2 = foldl (+) 0 counts
    print(answer2)

-- Descending bricks.

-- For a given pile, returns all bricks in their resting position.
descend_all :: [Brick] -> [Brick]
descend_all bricks = Map.elems (descend_map bricks) 

-- For a pile and brick, returns the number of other bricks that would
-- fall further if that brick were to be removed.
descend_count :: [Brick] -> Brick -> Int
descend_count bricks brick = length moved_bricks
    where
        others = filter (\b -> b /= brick) bricks
        changes = descend_map others
        has_moved (b1, b2) = top b1 /= top b2
        moved_bricks = filter has_moved (Map.toList changes)

-- Given a pile of bricks, returns a map from each brick's original position
-- to the brick's final position after all have fallen to rest.
descend_map :: [Brick] -> Map.Map Brick Brick
descend_map bricks = foldl descend Map.empty ordered
    where
        ordered = sortBy (\b1 b2 -> compare (bottom b1) (bottom b2)) bricks 
        new_level supports brick = 1 + supporting_z supports brick
        descend_to z ((x1,y1,z1), (x2,y2,z2)) = ((x1,y1,z), (x2,y2,z+z2-z1))
        descend acc brick = 
            Map.insert 
                brick 
                (descend_to (new_level (Map.elems acc) brick) brick) 
                acc

-- Returns the level that supports a brick after it has fallen to rest.
supporting_z :: [Brick] -> Brick -> Int
supporting_z bricks brick
    | null supports = 0
    | otherwise = supporting_level
    where
        range_x ((x1, _, _), (x2, _, _)) = (x1, x2)
        range_y ((_, y1, _), (_, y2, _)) = (y1, y2)
        overlaps (i1, i2) (j1, j2) = (i2 >= j1 && i1 <= j2) || (i1 <= j2 && i2 >= j1)
        is_blocker b =
            top b < bottom brick 
            && overlaps (range_x brick) (range_x b) 
            && overlaps (range_y brick) (range_y b)
        blockers_below b = filter is_blocker bricks
        supports = blockers_below brick
        highest_brick bs = maximumBy (\b1 b2 -> compare (top b1) (top b2)) bs
        supporting_level = top (highest_brick supports)

-- Parse bricks.

parseBricks :: [String] -> [Brick]
parseBricks strs = map parseBrick strs

parseBrick :: String -> Brick
parseBrick str = ((x1, y1, z1), (x2, y2, z2))
    where
        ns = splits [] "" str
        x1 = minimum [ns !! 0, ns !! 3]
        y1 = minimum [ns !! 1, ns !! 4]
        z1 = minimum [ns !! 2, ns !! 5]
        x2 = maximum [ns !! 0, ns !! 3]
        y2 = maximum [ns !! 1, ns !! 4]
        z2 = maximum [ns !! 2, ns !! 5]

splits :: [String] -> String -> String -> [Int]
splits acc cur "" = reverse (map (\s -> read s :: Int) (cur : acc))
splits acc cur (c:s)
    | c == ',' || c == '~' = splits (cur : acc) "" s
    | otherwise = splits acc (cur ++ [c]) s
