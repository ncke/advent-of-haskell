import Data.Char (ord)
import Data.List
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let cards = lines input
    print (answer1 cards)
    print (answer2 cards)

-- Part 1.

-- Finds the count of matching numbers on each card and converts to a
-- score. Returns the sum of the scores.
answer1 stack = foldl (+) 0 scores
    where
        cards = parse stack
        scores = map (score . matchCount) cards

-- Returns the count of numbers in the `haves` list that appear in
-- the `winners` list.    
matchCount (winners, haves) = 
    foldl (\m h -> m + if isMatch h then 1 else 0) 0 haves
    where
        isMatch n = (find (== n) winners) /= Nothing

-- Returns the score for a given number of matches.
score 0 = 0
score matches = 2 ^ (matches - 1)

-- Part 2.

-- Process the cards in the stack.
answer2 stack = process 0 [] cards where cards = parse stack

-- Accumulates a running total of cards. The number of instances of
-- a particular card is augmented by the number of duplicates (the first
-- element of the `copies` list, or zero if the list is empty). Any
-- matching numbers are written into the `copies` list for the relevant
-- number of future cards.
process acc copies [] = acc
process acc copies (card: cards) =
    process (acc + instances) copied cards
    where
        first [] = 0
        first (x:xs) = x
        instances = 1 + first copies
        matches = matchCount card
        copied = duplicate instances matches (drop 1 copies)
        
-- Applies the given `increase` to the first `count` elements of the
-- `copies` list. The list is extended to accommodate the increases
-- if necessary.
duplicate increase count copies =
    accumulated ++ (drop count copies)
    where
        element i = if i < length copies then copies !! i else 0
        accumulated = map (\i -> increase + element i) [0..count-1]

-- Parse cards.

parse stack = map parseCard stack

parseCard card = (winners, haves)
    where
        (lhs, rhs) = split '|' card
        (_, w) = split ':' lhs
        winners = map (\n -> read n :: Int) (words w)
        haves = map (\n -> read n :: Int) (words rhs)

split :: Char -> String -> (String, String)
split ch s = (lhs, drop 1 rhs) where (lhs, rhs) = break (== ch) s

splitAll :: Char -> String -> [String]
splitAll ch "" = []
splitAll ch s = lhs : (splitAll ch rhs) where (lhs, rhs) = split ch s
