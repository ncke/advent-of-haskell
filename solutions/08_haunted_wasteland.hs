import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let lns = lines input
    let directions = head lns
    let maps = parseMaps (drop 2 lns) 
    print (answer1 directions maps)
    print (answer2 directions maps)

-- Part 1.

-- Start at "AAA" and iterate through the nodes until the destination
-- is reached.
answer1 directions maps = steps maps 0 "AAA" directions directions

steps maps acc locn ds' [] = steps maps acc locn ds' ds'
steps maps acc locn ds' (d : ds) =
    if locn == "ZZZ" 
        then acc 
        else steps maps (acc + 1) (go maps d locn) ds' ds
     
-- Part 2.

-- Trying to iterate through from each starting node until every path
-- simultaneously reaches a destination node is futile. Instead, find 
-- the number of steps individually for each. The intuition is that the
-- paths will subsequently cycle so that destinations are reached at
-- integer multiples of that step count. Therefore, all nodes will
-- simultaneously reach a destination at the lowest common multiple of
-- the step counts of the individual paths. 
answer2 directions maps = result
    where
        count locn = ghostSteps maps 0 locn directions directions
        counts = map (\locn -> count locn) (ghostStarts maps)
        result = foldl (lcm) 1 counts

-- Iterate through the map until a destination (name ends in 'Z')
-- is reached.
ghostSteps maps acc locn ds' [] = ghostSteps maps acc locn ds' ds'
ghostSteps maps acc locn ds' (d: ds) =
    if 'Z' == locn !! 2 
        then acc 
        else ghostSteps maps (acc + 1) (go maps d locn) ds' ds

-- Find all of the starting nodes (name ends in 'A').
ghostStarts :: Map.Map [Char] b -> [[Char]]
ghostStarts maps = filter isStart allNodes
    where
        allNodes = map fst (Map.toList maps)
        isStart node = 'A' == node !! 2

-- Map usage.

getNode :: Map.Map String (String, String) -> String -> (String, String)
getNode maps label = fromMaybe ("---", "---") (Map.lookup label maps)

goLeft :: Map.Map String (String, String) -> String -> String
goLeft maps origin = fst (getNode maps origin)

goRight :: Map.Map String (String, String) -> String -> String
goRight maps origin = snd (getNode maps origin)

go :: Map.Map String (String, String) -> Char -> String -> String
go maps direction origin = case direction of
    'L' -> goLeft maps origin
    'R' -> goRight maps origin

-- Parse maps.

parseMaps lines = Map.fromList nodes
    where
        nodes = map parseLine lines

parseLine s = (wds !! 0, (wds !! 1, wds !! 2))
    where
        isPunc c = c == '=' || c == '(' || c == ')' || c == ','
        wds = words (filter (not . isPunc) s)
