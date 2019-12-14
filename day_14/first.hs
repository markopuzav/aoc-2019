import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace

type Reactions = Map String ([(Int, String)], Int)

getReactions :: [String] -> Reactions
getReactions rs = Map.fromList $ map (parse . (splitOn " => ")) rs
  where
    parse [is, r] = (name, (ings, read n))
      where
        [n, name] = splitOn " " r
        ings = [(read $ x !! 0, x !! 1) | x <- map (splitOn " ") $ splitOn ", " is]

transform :: Reactions -> Map String Int -> Map String Int
transform rs have
    -- | trace (">> " ++ show have) False = undefined
    | all (\(k,v) -> v <= 0 || k == "ORE") mhave  = have
    | otherwise = transform rs $ Map.fromListWith (+) $ expanded ++ rest
  where
    mhave = Map.toList have
    toExpand  = head [name | (name, need) <- mhave, name /= "ORE", need > 0]
    Just n    = Map.lookup toExpand have
    Just (ings, k) = Map.lookup toExpand rs
    mult      = ceiling (fromIntegral n / fromIntegral k)

    rest      = [(name, n) | (name, n) <- mhave, name /= toExpand]
    expanded  = [(nxt, nn * mult) | (nn, nxt) <- ings] ++ [(toExpand, n - k*mult)]

main :: IO()
main = do
  contents <- readFile "input"
  let reactions = getReactions $ lines contents
  let Just ore = Map.lookup "ORE" $ transform reactions (Map.fromList [("FUEL", 1)])
  print $ ore
