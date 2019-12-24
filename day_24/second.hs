import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map

type RecCell = Map Integer [String]

emptyGrid = [take 5 $ repeat '.' | _ <- [1..5]] :: [String]

evolveUntil :: Int -> RecCell -> RecCell
evolveUntil mins rc
    | mins == 0 = rc
    | otherwise = evolveUntil (mins - 1) (evolve rc)
  where
    lvl = maximum $ Map.keys rc
    evolve rc = Map.fromList $ [(lvl+1, emptyGrid), (-lvl-1, emptyGrid)] ++ [(i, nextGrid i) | i <- Map.keys rc]
    nextGrid i = [[nextBug x y i | y <- [0..4]] | x <- [0..4]]
    nextBug x y i
        | x == 2 && y == 2 = '.' -- middle tile
        | grid !! x !! y == '#' = if neigh x y i == 1          then '#' else '.'
        | grid !! x !! y == '.' = if neigh x y i `elem` [1, 2] then '#' else '.'
      where
        Just grid = i `Map.lookup` rc
        neigh x y i = sum [1 | (xx, yy, ii) <-
          (if x == 0 && i /= lvl then [(1, 2, i+1)] else []) ++
          (if x == 4 && i /= lvl then [(3, 2, i+1)] else []) ++
          (if y == 0 && i /= lvl then [(2, 1, i+1)] else []) ++
          (if y == 4 && i /= lvl then [(2, 3, i+1)] else []) ++
          (if (x, y) == (1, 2) && i /= (-lvl) then [(0, j, i-1) | j <- [0..4]] else []) ++
          (if (x, y) == (2, 1) && i /= (-lvl) then [(j, 0, i-1) | j <- [0..4]] else []) ++
          (if (x, y) == (2, 3) && i /= (-lvl) then [(j, 4, i-1) | j <- [0..4]] else []) ++
          (if (x, y) == (3, 2) && i /= (-lvl) then [(4, j, i-1) | j <- [0..4]] else []) ++
          (if x > 0 && (x, y) /= (3, 2) then [(x-1, y, i)] else []) ++
          (if x < 4 && (x, y) /= (1, 2) then [(x+1, y, i)] else []) ++
          (if y > 0 && (x, y) /= (2, 3) then [(x, y-1, i)] else []) ++
          (if y < 4 && (x, y) /= (2, 1) then [(x, y+1, i)] else []),
          let Just gg = ii `Map.lookup` rc,
          gg !! xx !! yy == '#']

countBugs :: RecCell -> Int
countBugs rc = sum [1 | i <- Map.keys rc, x <- [0..4], y <- [0..4], let Just grid = i `Map.lookup` rc, grid !! x !! y == '#']

main :: IO()
main = do
  contents <- readFile "input"
  let cell = Map.fromList [(0, lines contents), (-1, emptyGrid), (1, emptyGrid)] :: RecCell
  print $ countBugs $ evolveUntil 200 cell
