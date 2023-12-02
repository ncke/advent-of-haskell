import Data.Char (ord)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

main = do
    args <- getArgs
    input <- readFile (args !! 0)
    let answer = trebuchet (lines input)
    print answer

trebuchet :: [String] -> Int
trebuchet ss = foldl (+) 0 (map calibrate ss)

calibrate :: String -> Int
calibrate s = 10 * fwd + bwd
    where 
        fwd = fromJust (firstCalibration s fwdPrefixes)
        fwdPrefixes = (digitLetters, digitWords)
        bwd = fromJust (firstCalibration (reverse s) bwdPrefixes)
        bwdPrefixes = (map reverse digitLetters, map reverse digitWords)

firstCalibration :: String -> ([String], [String]) -> Maybe Int
firstCalibration [] _ = Nothing
firstCalibration (c:s) (p1,p2) = case (hasLetter, hasWord) of
    (Just n, _) -> Just n
    (_, Just n) -> Just n
    (Nothing, Nothing) -> firstCalibration s (p1,p2)
    where
        hasLetter = hasPrefix fullStr p1
        hasWord = hasPrefix fullStr p2 
        fullStr = [c] ++ s 

hasPrefix :: String -> [String] -> Maybe Int
hasPrefix s prefixes = matchIdx s (zip [0..] prefixes)
    where
        match s prefix = take (length prefix) s == prefix
        matchIdx _ [] = Nothing
        matchIdx s ((idx, prefix):ps) = 
            if match s prefix then Just idx else matchIdx s ps
        
digitLetters :: [String]
digitLetters = [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

digitWords :: [String]
digitWords = [ 
    "zero", "one", "two", "three", "four", "five", 
    "six", "seven", "eight", "nine" ]

