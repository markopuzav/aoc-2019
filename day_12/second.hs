import           Data.Set (Set)
import qualified Data.Set as Set
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

findCycle :: [Moon] -> Int -> Int
findCycle ms c = case c of
    0 -> untilRepeat [(p, v) | ((p,_,_),(v,_,_)) <- ms] 0 Set.empty
    1 -> untilRepeat [(p, v) | ((_,p,_),(_,v,_)) <- ms] 0 Set.empty
    2 -> untilRepeat [(p, v) | ((_,_,p),(_,_,v)) <- ms] 0 Set.empty
  where
    sgn a b = if a < b then 1 else if a > b then -1 else 0
    untilRepeat coors n s
        | coors `Set.member` s = n
        | otherwise = untilRepeat (map (update coors) coors) (n + 1) (coors `Set.insert` s)
    update coors (p, v) = (p + newv, newv)
      where newv = v + sum [sgn p pp | (pp, _) <- coors]

main :: IO()
main = do
  contents <- readFile "input"
  let moons = getMoons $ lines contents
  print $ foldl lcm 1 [findCycle moons i | i <- [0,1,2]]
