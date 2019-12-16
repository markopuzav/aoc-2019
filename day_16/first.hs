import Data.Char

getList :: String -> [Int]
getList s = map digitToInt s

fft :: [Int] -> [Int] -> Int -> Int -> [Int]
fft xs pattern phases offset = last $ take (phases+1) $ iterate transform xs
  where
    mask i = drop offset $ concat [replicate i x | x <- cycle pattern]
    _transform i xs = (`mod` 10) $ abs $ sum $ map (uncurry (*)) $ zip (mask i) xs
    transform xs = [_transform i xs | i <- [1..length xs]]

main :: IO()
main = do
  contents <- readFile "input"
  let list = getList $ head $ lines contents
  print $ foldr (++) "" $ map show $ take 8 $ fft list [0,1,0,-1] 100 1
