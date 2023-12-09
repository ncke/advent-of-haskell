import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let hands = parseHands (lines input)
    print (answer1 hands)
    print (answer2 hands)

-- Part 1.

-- Summation using ordinary precedence and 'J' counts as 11.
answer1 :: [(String, Int)] -> Int
answer1 hands = sumBids precedence 11 hands

sumBids pFunc jValue hands = summed
    where
        comparator = compareHands pFunc jValue
        sorted = sortBy (\h1 h2 -> (comparator) (fst h1) (fst h2)) hands
        bids = map snd sorted
        zipped = zip bids [1..]
        summed = foldl (\acc (bid, rank) -> acc + bid * rank) 0 zipped

-- Part 2.

-- Summation using joker precedence and 'J' counts as 1.
answer2 :: [(String, Int)] -> Int
answer2 hands = sumBids precedenceJokers 1 hands

-- Hand ordering.

-- Use the given precedence function and joker value to compare two hands.
compareHands pFunc jValue h1 h2 | p1 > p2 = GT
                                | p1 < p2 = LT
                                | p1 == p2 = compc (zip h1 h2)
    where
        p1 = pFunc h1
        p2 = pFunc h2
        compc [] = EQ
        compc ((c1, c2) : cs) = if c1 == c2 
            then compc cs 
            else compareCards jValue c1 c2

-- Compare two cards, using the given 'jValue' for 'J' cards.
compareCards jValue c1 c2 | v1 > v2 = GT
                          | v1 < v2 = LT
                          | v1 == v2 = EQ
    where
        cardValue c = case c of
            'A' -> 14
            'K' -> 13
            'Q' -> 12
            'J' -> jValue
            'T' -> 10
            _ -> read [c] :: Int
        v1 = cardValue c1
        v2 = cardValue c2

-- Recognise the precedence of the types of hand.
precedence hand =
    if occurs 5 then 900                            -- Five of a kind.
    else if occurs 4 then 800                       -- Four of a kind.
    else if occurs 3 && distincts == 2 then 700     -- Full house.
    else if occurs 3 then 600                       -- Three of a kind.
    else if occurs 2 && distincts == 3 then 500     -- Two pair.
    else if occurs 2 then 400                       -- One pair.
    else 300                                        -- High card. 
    where
        (cbins, nbins) = binned hand
        occurs n = Map.lookup n nbins /= Nothing
        distincts = length cbins

-- Reformulate the hand so that jokers supplement the most frequently
-- occurring non-joker card, e.g. 'AAJJ2' (two pair) becomes 'AAAA2'
-- (four of a kind). Then find the precedence of the reformulated hand.
precedenceJokers hand = 
    if jokerCount == 5 then precedence "AAAAA" else precedence augmented
    where
        exJokers = filter (/= 'J') hand
        jokerCount = length hand - length exJokers
        (cbins, _) = binned exJokers
        mostFrequent = maximumBy (comparing snd) (Map.toList cbins)
        augmented = exJokers ++ replicate jokerCount (fst mostFrequent)

-- For the given string, returns a map from each distinct character to
-- its frequency (the cbin), and a map from each frequency to an array of
-- the relevant characters (the nbin).
binned :: String -> (Map.Map Char Int, Map.Map Int [Char])
binned s = (cbins, nbins) 
    where
        integrated b c = case Map.lookup c b of
            Just n -> Map.insert c (n + 1) b
            Nothing -> Map.insert c 1 b
        inverted i c n = case Map.lookup n i of
            Just x -> Map.insert n (c : x) i
            Nothing -> Map.insert n [c] i
        cbins = foldl (\b c -> integrated b c) Map.empty s
        nbins = foldl (\i (c, n) -> inverted i c n) Map.empty (Map.toList cbins)

-- Parse hands.

parseHands lines = map parseLine lines

parseLine line = (cards, bid) 
    where 
        wds = words line
        cards = wds !! 0
        bid = read (wds !! 1) :: Int
