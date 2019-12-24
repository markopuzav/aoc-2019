import           Data.Set (Set)
import qualified Data.Set as Set

type Cell = String

evolve :: Int -> Cell -> Cell
evolve n c = map next [0..length c - 1]
  where
    next i
      | c !! i == '#' = if neigh i == 1          then '#' else '.'
      | c !! i == '.' = if neigh i `elem` [1, 2] then '#' else '.'
    neigh i = sum [1 | let (x, y) = quotRem i n,
                      (a, b) <- [(x-1, y), (x+1, y), (x, y+1), (x, y-1)],
                      a `elem` [0..n-1], b `elem` [0..n-1],
                      c !! (n*a + b) == '#']

evolveUntilRepeat :: Int -> Cell -> Set Cell -> Cell
evolveUntilRepeat n c visited
    | c `Set.member` visited = c
    | otherwise = evolveUntilRepeat n (evolve n c) (c `Set.insert` visited)

biodiversity :: Cell -> Int
biodiversity c = sum [pw | (pw, x) <- zip [2^i | i <- [0..]] c, x == '#']

main :: IO()
main = do
  contents <- readFile "input"
  let cell = concat $ lines contents
  print $ biodiversity $ evolveUntilRepeat 5 cell Set.empty
