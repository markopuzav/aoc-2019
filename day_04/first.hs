import Data.List

isPassword :: String -> Bool
isPassword s =    (any ((> 1) . length) $ group s)
               && (all (\(x, y) -> x <= y) $ zip s (tail s))

main = do
  print $ length $ filter (isPassword . show) [359282..820401]
