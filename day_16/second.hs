import Data.Char
import Debug.Trace

getList :: String -> [Int]
getList s = map digitToInt s

dirtyHack :: [Int] -> Int -> [Int]
dirtyHack xs phases = last $ take (phases+1) $ iterate transform xs
  where
    transform xs
        | sum(xs) < 0 = undefined
        | otherwise = f (sum xs) xs
      where
        f :: Int -> [Int] -> [Int]
        f _ [] = []
        f s (x:xs) = ((abs s) `mod` 10: f (s-x) xs)

main :: IO()
main = do
  contents <- readFile "input"
  let list = getList $ head $ lines contents
  let offset = read $ foldr (++) "" $ map show $ take 7 list :: Int
  let remainder = drop offset $ take (10000 * length list) $ cycle list
  print $ foldr (++) "" $ map show $ take 8 $ dirtyHack remainder 100
