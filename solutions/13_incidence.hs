import Data.List
import System.Environment (getArgs)

type Mirror = ([String], [String])

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let mirrors = parse (lines input)

    print(answer1 mirrors)

-- Answer 1.

answer1 :: [Mirror] -> Int
answer1 ms = foldl (+) 0 [contribution (m1, m2) | (m1, m2) <- ms]

contribution (m1, m2) = case (symmetryLine m1, symmetryLine m2) of
    (Just n, Nothing) -> 100 * (n + 1)
    (Nothing, Just n) -> (n + 1)

symmetryLine :: [String] -> Maybe Int
symmetryLine strs = find (isSymmetry strs) [0..(length strs)-2]

isSymmetry :: [String] -> Int -> Bool
isSymmetry strs n = (take smallest before) == (take smallest after)
    where
        before = [strs !! i | i <- reverse [0..n]]
        after = [strs !! i | i <- [n + 1..(length strs) - 1]]
        smallest = min (length before) (length after)

-- Answer 2.



-- Parse input.

parse :: [String] -> [Mirror]
parse strs = [(n, rotateNote n) | n <- parseNotes [] [] strs]

parseNotes :: [[String]] -> [String] -> [String] -> [[String]]
parseNotes acc note [] = acc ++ [note]
parseNotes acc note (s : strs)
    | null s = parseNotes (acc ++ [note]) [] strs
    | otherwise = parseNotes acc (note ++ [s]) strs

rotateNote :: [String] -> [String]
rotateNote strs = [col x | x <- [0..xSz-1]]
    where
        (xSz, ySz) = (length (strs !! 0), length strs)
        at x y = (strs !! y) !! x
        col x = [ at x y | y <- [0..ySz-1]]
