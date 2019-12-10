import qualified Data.Set as S
import Data.List.Split
import Data.List
import Data.Maybe

type Point = (Int, Int)
type Path  = [Point]

toPath :: [String] -> Path
toPath instructions = reverse $ foldl move [(0, 0)] instructions
  where
    move (p:rest) (dir:dist) = reverse (take (1 + read dist) $ iterate f p) ++ rest
      where
        f = case dir of
          'R' -> (\(x, y) -> (x + 1, y))
          'L' -> (\(x, y) -> (x - 1, y))
          'U' -> (\(x, y) -> (x, y + 1))
          'D' -> (\(x, y) -> (x, y - 1))

intersections :: Path -> Path -> [Point]
intersections a b = tail . S.toList $ S.intersection (S.fromList a) (S.fromList b)

closestManhattanDistance :: [Point] -> Int
closestManhattanDistance pts = minimum $ map (\(x, y) -> abs x + abs y) pts

closestCommonStepDistance :: [Point] -> [Point] -> [Point] -> Int
closestCommonStepDistance a b pts = minimum $ map
          (\p -> fromMaybe 0 (elemIndex p a) + fromMaybe 0 (elemIndex p b)) pts

main = do
  contents <- readFile "input"
  let wire1:wire2:_ = map (toPath . (splitOn ",")) $ lines contents
  let ints = intersections wire1 wire2
  print $ closestManhattanDistance ints
  print $ closestCommonStepDistance wire1 wire2 ints
