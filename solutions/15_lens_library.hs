import Data.Char (ord)
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let steps = parseSteps(input)
    
    let sum = sumOfHashes(steps)
    print sum

    let line = runSteps steps
    let power = linePower line
    print power

-- Sum of hashes.

sumOfHashes :: [String] -> Int
sumOfHashes steps = foldl (+) 0 (map hash steps)

hash :: String -> Int
hash s = foldl hashChar 0 s
    where
        hashChar h c = ((h + ord c) * 17) `mod` 256

-- Evaluate power of line.

linePower :: Map.Map Int [String] -> Int
linePower line = foldl (+) 0 boxPowers
    where
        boxPowers = [boxPower k v | (k, v) <- Map.toList line]

boxPower :: Int -> [String] -> Int
boxPower boxNumber lenses = foldl (+) 0 (map power enumerated)
    where
        power (slotNumber, lens) = lensPower boxNumber slotNumber lens
        enumerated = zip [0..] lenses

lensPower :: Int -> Int -> String -> Int
lensPower boxNumber slotNumber lens =
    (boxNumber + 1) * (slotNumber + 1) * (focalLength lens)

-- Construct line.

runSteps :: [String] -> Map.Map Int [String]
runSteps steps = foldl runStep Map.empty steps

runStep :: Map.Map Int [String] -> String -> Map.Map Int [String]
runStep line step
    | isRemoval step = removed
    | alreadyPresent = replaced
    | otherwise = added
    where
        boxNumber = hash (lensLabel step)
        box = fromMaybe [] (Map.lookup boxNumber line)
        alreadyPresent = containsLens box step
        removed = Map.insert boxNumber (removeLens box step) line
        replaced = Map.insert boxNumber (replaceLens box step) line
        added = Map.insert boxNumber (addLens box step) line

containsLens :: [String] -> String -> Bool
containsLens box query = (find isSame box) /= Nothing
    where
        isSame lens = (lensLabel lens) == (lensLabel query)

addLens :: [String] -> String -> [String]
addLens box add = box ++ [add]

replaceLens :: [String] -> String -> [String]
replaceLens box replace = map repl box
    where
        replaceLabel = lensLabel replace
        repl lens = if (lensLabel lens) == replaceLabel then replace else lens

removeLens :: [String] -> String -> [String]
removeLens box remove = filter isNotSame box
    where
        isNotSame lens = (lensLabel lens) /= (lensLabel remove)

isRemoval :: String -> Bool
isRemoval step = (last step) == '-'

lensLabel :: String -> String
lensLabel step = if (last partial) == '=' then init partial else partial
    where
        partial = init step

focalLength :: String -> Int
focalLength lens = read length :: Int
    where
        (name, length) = split '=' lens

split :: Char -> String -> (String, String)
split ch s = (lhs, drop 1 rhs) where (lhs, rhs) = break (== ch) s

-- Parse input.

parseSteps :: String -> [String]
parseSteps str = reverse (splitCommas [""] str)

splitCommas :: [String] -> String -> [String]
splitCommas result [] = result
splitCommas result (c:s)
    | c == ',' = splitCommas ([] : result) s
    | otherwise = splitCommas (((head result) ++ [c]) : (tail result)) s
