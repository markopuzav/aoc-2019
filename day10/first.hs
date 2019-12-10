import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)
type Vector = (Int, Int)

getAsteroids :: [String] -> [Point]
getAsteroids ls = [(i, j) | (i, l) <- zip [0..] ls, (j, ch) <- zip [0..] l, ch == '#']

normalize :: Vector -> Vector
normalize (0, 0) = (0, 0)
normalize (x, y)
    | otherwise = (div x g, div y g)
  where g = abs $ gcd x y

sqSize :: Point -> Int
sqSize (fx, fy) = fx^2 + fy^2

vectorSort :: Point -> Point -> Ordering
vectorSort a b
    | sqSize a < sqSize b = LT
    | sqSize a > sqSize b = GT

vectorMap :: Point -> [Point] -> Map Point [Point]
vectorMap o ps = Map.fromListWith (++) [(normalize $ vct o p, [vct o p]) | p <- ps, p /= o]
  where vct (fx, fy) (tx, ty) = (tx - fx, ty - fy)

reachableAsteroids :: Point -> [Point] -> [Point]
reachableAsteroids p asts = Map.elems mClosest
  where
    m = vectorMap p asts
    mClosest = Map.map (head . (sortBy vectorSort)) m

findBase :: [Point] -> (Int, Point)
findBase asts = maximum [(length $ reachableAsteroids a asts, a) | a <- asts]

main :: IO()
main = do
  contents <- readFile "input"
  let asteroids = getAsteroids $ lines contents
  print $ fst $ findBase asteroids
