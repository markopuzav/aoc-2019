import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List
import           Data.List.Split
import           Data.Maybe

type ParentDict = Map String String

-- Each element has only one parent
getOrbits :: [String] -> ParentDict
getOrbits ls = Map.fromList $ map (tuplefy .(splitOn ")")) ls
  where tuplefy (x:y:xs) = (y, x)

getPath :: ParentDict -> String -> [String]
getPath p "COM" = ["COM"]
getPath p s = (takeWhile (/= "COM") $ iterate (\s -> fromMaybe "" $ Map.lookup s p) s) ++ ["COM"]

commonDistance :: [String] -> [String] -> Int
commonDistance p1 p2 = getIndex c p1 + getIndex c p2 - 2
  where
    c = closestCommonAncestor p1 p2
    closestCommonAncestor (x:xs) ys = if x `elem` ys then x else closestCommonAncestor xs ys
    getIndex x p = fromMaybe 0 $ elemIndex x p

main :: IO()
main = do
  contents <- readFile "input"
  let parents = getOrbits $ lines contents
  print $ commonDistance (getPath parents "YOU") (getPath parents "SAN")
