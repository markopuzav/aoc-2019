import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List.Split
import           Data.Maybe

type ParentDict = Map String String

-- Each element has only one parent
getOrbits :: [String] -> ParentDict
getOrbits ls = Map.fromList $ map (tuplefy .(splitOn ")")) ls
  where tuplefy (x:y:xs) = (y, x)

level :: ParentDict -> String -> Int
level p "COM" = 0
level p s = length $ takeWhile (/= "COM") $ iterate (\s -> fromMaybe "" $ Map.lookup s p) s

main :: IO()
main = do
  contents <- readFile "input"
  let parents = getOrbits $ lines contents
  print $ sum $ map (level parents) $ map fst $ Map.toList parents
