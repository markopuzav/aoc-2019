import Data.List.Split
import Data.List
import Data.Maybe
import Debug.Trace

type Point = (Int, Int)
type Segment = (Point, Point)

intersections :: Segment -> Segment -> [Point]
intersections ((ax, ay), (bx, by)) ((cx, cy), (dx, dy)) =
    [(x, y) | (x, y) <- ab,
                (cx == dx && x == cx && y <= max cy dy && y >= min cy dy) ||
                (cy == dy && y == cy && x <= max cx dx && x >= min cx dx)]
  where
    ab = if ax == bx then [(ax, y) | y <- [min ay by .. max ay by]]
                     else [(x, ay) | x <- [min ax bx .. max ax bx]]

convertToCoordinates :: [String] -> [Point]
convertToCoordinates wire = reverse $ foldl move [(0, 0)] wire
  where
    move suffix instruction
        -- | trace ("rest = " ++ show rest) False = undefined
        | dir == 'R' = (x + by, y):suffix
        | dir == 'L' = (x - by, y):suffix
        | dir == 'U' = (x, y + by):suffix
        | dir == 'D' = (x, y - by):suffix
      where
        dir:dist = instruction
        by = read dist
        (x, y):rest = suffix

findIntersections :: [Point] -> [Point] -> [Point]
findIntersections wire1 wire2 = concat
    [intersections s1 s2 | s1 <- segments wire1, s2 <- segments wire2]
  where
    segments (x:xs) = zip (x:xs) xs

stepsUntil :: [Point] -> Point -> Int
stepsUntil wire p
  -- | trace ("path = " ++ show path) False = undefined
  -- | trace ("segments = " ++ show (segments wire)) False = undefined
  | otherwise = fromMaybe 0 $ elemIndex p path
  where
    segments (x:xs) = zip (x:xs) xs
    path = concat [
        if ax == bx then [(ax, y) | y <- [ay, ay+(signum$by-ay) .. by-(signum$by-ay)]]
        else             [(x, ay) | x <- [ax, ax+(signum$bx-ax) .. bx-(signum$bx-ax)]]
          | ((ax, ay), (bx, by)) <- segments wire]

closestCombinedDistance :: [Point] -> [Point] -> Int
closestCombinedDistance w1 w2 = minimum $ filter (/= 0) $
    [stepsUntil w1 p + stepsUntil w2 p | p <- findIntersections w1 w2]

main = do
  contents <- readFile "input"
  let wire1 : wire2 : _ = map (convertToCoordinates . (splitOn ","))
                          $ splitOn "\n" contents
  print $ closestCombinedDistance wire1 wire2
