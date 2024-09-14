import Data.Maybe (catMaybes)
import System.Environment (getArgs)

-- 3d Vector.

type Vec3 = (Double, Double, Double)

-- Position and velocity.
type Hailstone = (Vec3, Vec3)

position :: Hailstone -> Double -> (Double, Double, Double)
position ((x, y, z), (i, j, k)) t = (x + t * i, y + t * j, z + t * k)

flatten :: Hailstone -> Hailstone
flatten ((x, y, z), (i, j, k)) = ((x, y, 0), (i, j, 0))

-- Opposing corners.
type Box = (Vec3, Vec3)

isInside :: Box -> (Double, Double, Double) -> Bool
isInside ((c1x, c1y, c1z), (c2x, c2y, c2z)) (x, y, z) =
    x >= c1x && x <= c2x
    && y >= c1y && y <= c2y
    && z >= c1z && z <= c2z

-- Main.

main :: IO()
main = do
    args <- getArgs
    input <- readFile (args !! 0)

    let stones = parseStones (lines input)
    let flatStones = map flatten stones

    let example_box = ((7, 7, 0), (27, 27, 0))
    let boundary1 = 200000000000000
    let boundary2 = 400000000000000
    let real_box = ((boundary1, boundary1, 0), (boundary2, boundary2, 0))

    let a1 = answer1 real_box flatStones
    print(a1)

-- Answer 1.

answer1 :: Box -> [Hailstone] -> Int
answer1 box (h : hs) = (countIntersections box h hs) + (answer1 box hs)
answer1 box [] = 0

countIntersections :: Box -> Hailstone -> [Hailstone] -> Int
countIntersections box h hs = length intersections
    where
        results = map (hasIntersection box h) hs
        intersections = filter (True ==) results

hasIntersection :: Box -> Hailstone -> Hailstone -> Bool
hasIntersection box h1 h2 =
    intersectTime1 >= 0.0 
    && intersectTime2 >= 0.0 
    && isInside box intersectPosn
    where
        intersectTime1 = intersectionTime h1 h2
        intersectTime2 = intersectionTime h2 h1
        intersectPosn = position h1 intersectTime1
        
intersectionTime :: Hailstone -> Hailstone -> Double
intersectionTime h1 h2 = num / den
    where
        ((x1, y1, _), (i1, j1, _)) = h1
        ((x2, y2, _), (i2, j2, _)) = h2
        num = (y2 - y1) - ((x2 - x1) * j2) / i2
        den = j1 - (i1 * j2) / i2

-- Parse hailstones.

parseStones :: [String] -> [Hailstone]
parseStones strs = map parseStone strs

parseStone :: String -> Hailstone
parseStone s = (vectorise lnn, vectorise rnn)
    where
        (lhs, rhs) = split '@' (decomma s)
        (lnn, rnn) = (splitAll ' ' lhs, tail (splitAll ' ' rhs)) 
        decomma = filter (',' /=)
        vectorise [x, y, z] = (
            read x :: Double, 
            read y :: Double, 
            read z :: Double)

split :: Char -> String -> (String, String)
split ch s = (lhs, drop 1 rhs) where (lhs, rhs) = break (== ch) s

splitAll :: Char -> String -> [String]
splitAll ch "" = []
splitAll ch s = lhs : (splitAll ch rhs) where (lhs, rhs) = split ch s
