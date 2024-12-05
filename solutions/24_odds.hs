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
    print a1 

    let ps = pairs [] stones
    print("number of pairs", length ps)
    
    let h0 = stones !! 0
    let h1 = stones !! 1

    let sub = take 7 ps
    print("problem", last sub)
    let (pr1, pr2) = last sub
    print("problem 1", pr1)
    print("problem 2", pr2)
    
    let vs = map (listVelocities 1) sub
    print h0
    print h1
    print vs


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

-- Answer 2.

pairs :: [Hailstone] -> [Hailstone] -> [(Hailstone, Hailstone)]
pairs js (h : hs) = [ (h, other) | other <- (js ++ hs) ] ++ (pairs (h : js) hs)
pairs _ [] = []

remainders :: [Hailstone] -> (Hailstone, Hailstone) -> [Hailstone]
remainders hs (h1, h2) = filter (\h -> h /= h1 && h /= h2) hs 

aligns :: Int -> Int -> (Hailstone, Hailstone) -> Bool
aligns ti t (h1, h2) = True
    where
        tt = fromIntegral (ti + t)
        p1 = position h1 tt
        p2 = position h2 tt

listVelocities :: Int -> (Hailstone, Hailstone) -> [(Int, Int, Int)]
listVelocities ti (h1, h2) = velocities
    where
        ((x1, y1, z1), (i1, j1, k1)) = integralize h1
        ((x2, y2, z2), (i2, j2, k2)) = integralize h2
        tn = ti + 1
        (p1x, p1y, p1z) = (x1 + ti * i1, y1 + ti * j1, z1 + ti * k1)
        (p2x, p2y, p2z) = (x2 + tn * i2, y2 + tn * j2, z2 + tn * k2)
        (vx, vy, vz) = (p2x - p1x, p2y - p1y, p2z - p1z)
        factors = [1] ++ factors3 2 vx vy vz
        velocity n = (div vx n, div vy n, div vz n)
        velocities = map velocity factors

factors3 :: Int -> Int -> Int -> Int -> [Int]
factors3 n a b c =
    if n > limit then []
    else (if isFactor then [n] else []) ++ factors3 (n + 1) a b c
    where
        limit = min (min a b) c  
        fa = mod a n == 0
        fb = mod b n == 0
        fc = mod c n == 0
        isFactor = fa && fb && fc

integralize h = ((xi, yi, zi), (ii, ji, ki))
    where
        ((x, y, z), (i, j, k)) = h
        xi = round x
        yi = round y
        zi = round z
        ii = round i
        ji = round j
        ki = round k

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
