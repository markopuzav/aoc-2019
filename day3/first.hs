import Data.List.Split
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
    cd = if cx == dx then [(cx, y) | y <- [min cy dy .. max cy dy]]
                     else [(x, cy) | x <- [min cx dx .. max cx dx]]

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

findMinDistance :: [Point] -> Int
findMinDistance intersections =
      minimum [abs x + abs y | (x, y) <- intersections, x /= 0 || y /= 0]

main = do
  contents <- readFile "input"
  let wire1 : wire2 : _ = map (convertToCoordinates . (splitOn ","))
                          $ splitOn "\n" contents
  let intersections = findIntersections wire1 wire2
  print $ findMinDistance intersections
