import Data.List
import Data.Maybe
import System.Environment (getArgs)

type Mirror = [String]

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let mirrors = parse (lines input)

    print(answer1 mirrors)
    print(answer2 mirrors)

    let mex = ["#.", ".#"]
    print(smudges mex)

    let r = map (\m -> (symmetry m, symmetry (rotateMirror m))) mirrors
    let r' = map (\m -> (symmetry' m, symmetry' (rotateMirror m))) mirrors
    print(r)
    print(r')

    let e = map excludedEvaluate2 mirrors
    print(e)

-- Answer 1.

answer1 :: [Mirror] -> Int
answer1 ms = foldl (+) 0 (map evaluate1 ms)

evaluate1 :: Mirror -> Int
evaluate1 m = case (symmetry m, symmetry (rotateMirror m)) of
    (Just n, Nothing) -> 100 * (n + 1)
    (Nothing, Just n) -> (n + 1)

symmetry :: Mirror -> Maybe Int
symmetry strs = find isSymmetry [0..(length strs)-2]
    where
        before n = [strs !! i | i <- reverse [0..n]]
        after n = [strs !! i | i <- [n + 1..(length strs) - 1]]
        least n = min (length (before n)) (length (after n))
        isSymmetry n = take (least n) (before n) == take (least n) (after n)

rotateMirror :: Mirror -> Mirror
rotateMirror strs = [col x | x <- [0..xSz-1]]
    where
        (xSz, ySz) = (length (strs !! 0), length strs)
        at x y = (strs !! y) !! x
        col x = [ at x y | y <- [0..ySz-1]]

-- Answer 2.

answer2 :: [Mirror] -> Int
answer2 ms = foldl (+) 0 (map evaluate2 ms)

evaluate2 :: Mirror -> Int
--evaluate2 m = case (symmetry' m, symmetry' (rotateMirror m)) of
--    ([n], []) -> 100 * (n + 1)
--    ([], [n]) -> (n + 1)
--    (a, b) -> 9000000
evaluate2 m = case (excludedEvaluate2 m) of
    ([n], []) -> 100 * (n + 1)
    ([], [n]) -> (n + 1)
    (a, b) -> 9000000

excludedEvaluate2 m = case (newUnrotateds, newRotateds) of
    ([], [b]) -> ([], [b])
    ([a], []) -> ([a], [])
    (aa, bb) -> (excludedUnrotated, excludedRotated)
    where
        oldUnrotated = symmetry m
        newUnrotateds = symmetry' m
        excludedUnrotated = filter (\n -> Just n /= oldUnrotated) newUnrotateds
        oldRotated = symmetry (rotateMirror m)
        newRotateds = symmetry' (rotateMirror m)
        excludedRotated = filter (\n -> Just n /= oldRotated) newRotateds

symmetry' :: Mirror -> [Int]
symmetry' m = newReflections
    where
        smugeds = smudges m
        newReflections = nub (catMaybes (map symmetry smugeds))

smudges :: Mirror -> [Mirror]
smudges m = [ensmudged m (x, y) | x <- [0..xSz-1], y <- [0..ySz-1]]
    where
        (xSz, ySz) = (length (m !! 0), length m)

ensmudged :: Mirror -> (Int, Int) -> Mirror
ensmudged m (x, y) = [row j | j <- [0..ySz-1]]
    where
        (xSz, ySz) = (length (m !! 0), length m)
        invert ch = if ch == '#' then '.' else '#' 
        smudged r = [if i == x then invert (r !! i) else (r !! i) | i <- [0..xSz-1]]
        row j = if j == y then smudged (m !! j) else (m !! j)

-- Parse input.

parse :: [String] -> [Mirror]
parse strs = parseNotes [] [] strs

parseNotes :: [Mirror] -> [String] -> [String] -> [Mirror]
parseNotes acc note [] = acc ++ [note]
parseNotes acc note (s : strs)
    | null s = parseNotes (acc ++ [note]) [] strs
    | otherwise = parseNotes acc (note ++ [s]) strs
