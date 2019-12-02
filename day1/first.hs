getNumbers :: String -> [Int]
getNumbers str = map read $ words str

fuel :: Int -> Int
fuel n = n `div` 3 - 2

main = do
  contents <- readFile "input"
  let numbers = getNumbers contents
  print $ sum $ map fuel numbers
