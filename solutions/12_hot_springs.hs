import Data.List
import qualified Data.Set as Set
import System.Environment (getArgs)

type Coord = (Int, Int)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let records = parseRecords (lines input)
    print (answer1 records)
    print (answer2 records)

-- Part 1.

answer1 records = foldl (+) 0 (all records)
    where
        all = map (\ (s, k) -> length (ways k s))

ways :: [Int] -> String -> [String]
ways k s
    | dmgs == 0 && spcs == 0 && groups s == k = [s]
    | otherwise = 
        (if dmgs > 0 then recurse '#' else [])
            ++ (if spcs > 0 then recurse '.' else [])
    where
        dmgs = (foldl (+) 0 k) - count '#'
        spcs = (count '?') - dmgs
        recurse r = ways k (replaceFirst r s)
        count c = length (filter (==c) s)
        spaces = map (\c -> if c == '.' then ' ' else c)
        groups = map length . words . spaces

replaceFirst r (c : cs)
    | c == '?' = r : cs
    | otherwise = [c] ++ replaceFirst r cs

-- Part 2.
answer2 records = 42 -- (answer1 . unfold) records

unfold = map (\(s, k) -> (sunfold s, kunfold k))
sunfold = (intercalate "?") . (replicate 5)
kunfold = (foldl (++) []) . (replicate 5)

-- Parse records.

parseRecords lines = map parseLine lines

parseLine line = (str, nums (wds key))
    where
        (str, key) = (words line !! 0, words line !! 1)
        wds = words . map (\c -> if c == ',' then ' ' else c)
        nums = map (\n -> read n :: Int) 
