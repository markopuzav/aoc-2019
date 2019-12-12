import           Data.List.Split
import           Data.Char

type Pos = (Int, Int, Int)
type Vel = (Int, Int, Int)
type Moon = (Pos, Vel)

getMoons :: [String] -> [Moon]
getMoons ss = [(tuplefy $ parse s, (0, 0, 0)) | s <- ss]
  where
    parse s = map read $ splitOn " " $ filter (\ch -> isNumber ch || ch == ' ' || ch == '-') s
    tuplefy (a:b:c:_) = (a, b, c)

simulate :: [Moon] -> Int -> [Moon]
simulate ms n = iterate (updatePos . updateVel) ms !! n
  where
    updateVel ms = [((x,y,z),
      ( xx + sum [sgn x px | ((px,_,_),_) <- ms],
        yy + sum [sgn y py | ((_,py,_),_) <- ms],
        zz + sum [sgn z pz | ((_,_,pz),_) <- ms]
      )) | ((x,y,z), (xx,yy,zz)) <- ms]
    updatePos ms = [((x+xx, y+yy, z+zz), (xx, yy, zz)) | ((x,y,z),(xx,yy,zz)) <- ms]
    sgn a b = if a < b then 1 else if a > b then -1 else 0

totalEnergy :: [Moon] -> Int
totalEnergy ms = sum [potential m * kinetic m | m <- ms]
  where
    potential ((x,y,z),_) = abs x + abs y + abs z
    kinetic (_,(xx,yy,zz)) = abs xx + abs yy + abs zz

main :: IO()
main = do
  contents <- readFile "input"
  let moons = getMoons $ lines contents
  print $ totalEnergy $ simulate moons 1000
