
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import System.Environment (getArgs)

-- Graph.

type Graph t = Map.Map t [t]

disconnect :: Ord t => t -> t -> Graph t -> Graph t
disconnect v1 v2 g = removev2 (removev1 g)
    where
        remove x = filter (x /=)
        removev1 = Map.insert v1 (remove v2 (fromJust (Map.lookup v1 g)))
        removev2 = Map.insert v2 (remove v1 (fromJust (Map.lookup v2 g)))

-- Main.

main :: IO()
main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let wiring = parseWiring (lines input)
    print(wiring)


-- Parse the wiring diagram.

parseWiring :: [String] -> Graph String
parseWiring strs = graph
    where
        edges = foldl (\p s -> p ++ parseLine s) [] strs
        insert (origin, dest) = Map.insertWith (++) origin [dest]
        graph = foldl (\g pair -> insert pair g) Map.empty  edges

parseLine :: String -> [(String, String)]
parseLine str = outgoing ++ incoming
    where
        ws = words str
        origin = init (ws !! 0)
        dests = drop 1 ws
        outgoing = map (\d -> (origin, d)) dests
        incoming = map (\d -> (d, origin)) dests
