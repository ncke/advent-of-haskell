import Data.List
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let almanac = parse (lines input)
    print (answer1 almanac)
    print (answer2 almanac)

-- Part 1.

answer1 (seeds, tables) = minimum (compute seeds tables)

compute seeds [] = seeds
compute seeds (table : tables) = compute (map (tlookup table) seeds) tables

tlookup table seed = case convert seed table of
    Just conversion -> conversion
    Nothing -> seed

convert seed [] = Nothing
convert seed (row : rows) =
    if isInside then Just conversion else convert seed rows
    where
        destStart = row !! 0
        sourceStart = row !! 1
        len = row !! 2
        isInside = seed >= sourceStart && seed < sourceStart + len
        conversion = seed - sourceStart + destStart

-- Part 2.

-- The seed ranges are so huge that brute forcing each and every
-- constituent seed is hopeless. Instead, move each seed through the
-- tables as a range. When the seed range triggers a rule from the
-- table, the original range is split around the rule range. This adds
-- new ranges that are again carried forward. Once all tables have
-- been processed, find the lowest location among all of the range
-- fragments.
answer2 (seeds, tables) = lowest
    where
        prs = pairs [] seeds
        conversions = concatMap (\pr -> convertTables [pr] tables) prs
        lowest = minimum (map (\(s, l) -> s) conversions)

-- Progress the seeds through all tables, carrying forward new seeds
-- as they are generated.
convertTables seeds [] = seeds
convertTables seeds (table : tables) = convertTables carry tables
    where
        carry = convertSeeds table [] seeds

-- Progress the seeds through a specific table, gather new seeds
-- into the accumulator as the result.
convertSeeds table acc [] = acc
convertSeeds table acc (seed : seeds) = convertSeeds table carry seeds
    where
        carry = acc ++ convertTable seed [] table

-- Progress a specific single seed through all of the rows of a table,
-- gather new seeds into the accumulator as the result. If no table
-- rules are triggered (empty accumulator) then the seed passes
-- through unchanged. 
convertTable seed acc [] = if null acc then [seed] else acc
convertTable seed acc (row : rows) = trimSeeds
    where
        (ds, ts, tl) = (row !! 0, row !! 1, row !! 2)
        newSeeds = case partsRange seed ds ts tl of
            Just (before : inside : after : []) -> 
                [inside] 
                ++ (convertTable before [] rows) 
                ++ (convertTable after [] rows)
            Nothing -> convertTable seed acc rows
        trimSeeds = filter (\(s, l) -> l > 0) newSeeds

-- Convert a given seed range `(ss, sl)` through a rule `ds ts tl`.
-- Returns Nothing if the rule is not triggered, otherwise returns
-- a list containing new before, inside, and after ranges reflecting
-- how the seed range was split around the rule (nb: some of these
-- ranges may have zero length).
partsRange (ss, sl) ds ts tl = 
    if count > 0 then Just (before : inside : after : []) else Nothing
    where
        (bef, i1) = partsPivot ss sl ts
        (i2, aft) = partsPivot ss sl (ts + tl)
        count = sl - bef - aft
        before = (ss, bef)
        inside = (ss + bef + ds - ts, count)
        after = (ss + bef + count, aft)

-- For a range of length `len` starting at `start`, returns the size of the
-- range before the given `pivot` and the size equal or greater to it.
-- partsPivot :: Int -> Int -> Int -> (Int, Int)
partsPivot start len pivot = (before, after)
    where
        before = if start < pivot 
            then minimum [pivot - start, len] 
            else 0
        after = if start + len >= pivot 
            then minimum [start + len - pivot, len] 
            else 0

-- Convert the flat seed list into a list of pairs. Each pair contains
-- the start and length of a seed range.
pairs ps [] = ps
pairs ps (e1 : e2 : es) = pairs ((e1, e2) : ps) es

-- Parse almanac.

parse almanac = (seeds, reverse trimmed)
    where
        (_, seedList) = split ':' (head almanac)
        seeds = parseSeeds seedList
        tables = parseTables [] [] (drop 2 almanac)
        trimmed = filter (not . null) tables

parseSeeds line = map (\n -> read n :: Integer) (words line)

parseTables :: [[[Integer]]] -> [[Integer]] -> [String] -> [[[Integer]]]
parseTables tabs cur [] = cur : tabs
parseTables tabs cur (line : lines) =
    if isEmpty then parseTables tabs cur lines
    else if isHeading then parseTables (cur : tabs) [] lines
    else parseTables tabs (numbers : cur) lines
    where
        isEmpty = line == ""
        isHeading = (find (== ':') line) /= Nothing
        numbers = map (\n -> read n :: Integer) (words line)

split :: Char -> String -> (String, String)
split ch s = (lhs, drop 1 rhs) where (lhs, rhs) = break (== ch) s

splitAll :: Char -> String -> [String]
splitAll ch "" = []
splitAll ch s = lhs : (splitAll ch rhs) where (lhs, rhs) = split ch s
