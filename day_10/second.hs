import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map

type Point  = (Int, Int)
type Vector = (Int, Int)

getAsteroids :: [String] -> [Point]
getAsteroids ls = [(i, j) | (i, l) <- zip [0..] ls, (j, ch) <- zip [0..] l, ch == '#']

normalize :: Vector -> Vector
normalize (0, 0) = (0, 0)
normalize (x, y) = (div x g, div y g)
  where g = abs $ gcd x y

add :: Point -> Point -> Point
add (a,b) (c,d) = (a+c, b+d)

vectorSorter :: Point -> Point -> Ordering
vectorSorter a b
    | sqSize a < sqSize b = LT
    | sqSize a > sqSize b = GT
  where sqSize (fx, fy) = fx^2 + fy^2

-- maps the points to their normalized vector
normMap :: Point -> [Point] -> Map Vector [Point]
normMap o ps = Map.fromListWith (++) [(normalize $ vct o p, [vct o p]) | p <- ps, p /= o]
  where vct (fx, fy) (tx, ty) = (tx - fx, ty - fy)

reachableAsteroids :: Point -> [Point] -> [Point]
reachableAsteroids p asts = Map.elems $ Map.map (head . (sortBy vectorSorter)) m
  where m = normMap p asts

findBase :: [Point] -> (Int, Point)
findBase asts = maximum [(length $ reachableAsteroids a asts, a) | a <- asts]

-- sorts by angle:  if an asteroid (x, y) is w-th in its normalized-vector "line", it gets angle 2*pi*w + atan(-y, x)
destroyOrder :: Point -> [Point] -> [Vector]
destroyOrder base asts = map ((add base) . snd . snd) $ sort [(angle x, x) | e <- Map.elems mWind, x <- e]
  where
    m = normMap base asts
    mWind = Map.map ((zip [0..]) . (sortBy vectorSorter)) m -- adds winding numbers
    angle (w, (x, y)) = w*2*pi + atan2 (-fromIntegral y - 1e-9) (fromIntegral x)

nThDestroyed :: Point -> [Point] -> Int -> Point
nThDestroyed base asts n = (destroyOrder base asts) !! (n-1)

main :: IO()
main = do
  contents <- readFile "input"
  let asteroids = getAsteroids $ lines contents
  let base = snd $ findBase asteroids
  print $ (\(x,y) -> 100*y + x) $ nThDestroyed base asteroids 200
