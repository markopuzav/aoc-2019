getNumbers :: String -> [Int]
getNumbers str = map read $ words str

fuelfuelfuel :: Int -> Int
fuelfuelfuel n
    | n > 0       = next + fuelfuelfuel next
    | otherwise   = 0
  where next = max 0 $ n `div` 3 - 2

main = do
  contents <- readFile "input"
  let numbers = getNumbers contents
  print $ sum $ map fuelfuelfuel numbers
