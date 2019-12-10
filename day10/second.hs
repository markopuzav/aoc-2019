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

add :: Point -> Point -> Point
add (a,b) (c,d) = (a+c, b+d)

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

destroyOrder :: Point -> [Point] -> [Vector]
destroyOrder base asts = map (\(_,(_,x)) -> add x base) $ sort [(getAngle x, x) | e <- Map.elems mWinding, x <- e]
  where
    m = vectorMap base asts
    mWinding = Map.map ((zip [0..]) . (sortBy vectorSort)) m
    getAngle :: (Float, Point) -> Float
    getAngle (w, (x, y)) = w*2*pi + atan2 (-fromIntegral y - 1e-9) (fromIntegral x)

nThDestroyed :: Point -> [Point] -> Int -> Point
nThDestroyed base asts n = (destroyOrder base asts) !! (n-1)

main :: IO()
main = do
  contents <- readFile "input"
  let asteroids = getAsteroids $ lines contents
  let base = snd $ findBase asteroids
  print $ nThDestroyed base asteroids 200
