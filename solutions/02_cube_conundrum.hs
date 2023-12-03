import Data.List
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let answer = cubeConundrum (lines input)
    print answer

cubeConundrum :: [String] -> Int
cubeConundrum games = answer2 games

-- Part 1.

answer1 :: [String] -> Int
answer1 games = foldl (+) 0 (map score games)

score :: String -> Int
score game = if allAllowed then gameNumber else 0
    where
        (gameNumber, draws) = parseGame game
        disallowed (r, g, b) = r > 12 || g > 13 || b > 14
        allAllowed = (find (disallowed) draws) == Nothing
        
-- Part 2.

answer2 :: [String] -> Int
answer2 games = foldl (+) 0 (map (power . fewest) games)

power :: (Int, Int, Int) -> Int
power (r, g, b) = r * g * b

fewest :: String -> (Int, Int, Int)
fewest game = (maximum rs, maximum gs, maximum bs)
    where
        (gameNumber, draws) = parseGame game
        (rs, gs, bs) = Data.List.unzip3 draws

-- Game parsing.

parseGame :: String -> (Int, [(Int, Int, Int)])
parseGame game = (gameNumber, map parseDraw draws)
    where
        (gamePart, drawsPart) = split ':' game
        gameNumber = read (snd (split ' ' gamePart)) :: Int
        draws = splitAll ';' drawsPart

parseDraw :: String -> (Int, Int, Int)
parseDraw draw = (match 'r', match 'g', match 'b')
    where
        shows = map (drop 1) (splitAll ',' draw)
        parts = map (split ' ') shows
        match c = unwrap (find (\(_, rhs) -> head rhs == c) parts)
        unwrap x = case x of
            Just (n, _) -> read n :: Int
            Nothing -> 0
        
split :: Char -> String -> (String, String)
split ch s = (lhs, drop 1 rhs) where (lhs, rhs) = break (== ch) s

splitAll :: Char -> String -> [String]
splitAll ch "" = []
splitAll ch s = lhs : (splitAll ch rhs) where (lhs, rhs) = split ch s
