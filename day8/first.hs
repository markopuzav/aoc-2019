type Image = String
type Layer = String

layers :: Image -> (Int, Int) -> [Layer]
layers "" _ = []
layers img (w,h) = (take (w*h) img : layers (drop (w*h) img) (w, h))

smallestLayer :: [Layer] -> (Int, Int)
smallestLayer ls = minimum [(count l '0', count l '1' * count l '2') | l <- ls]
  where count s ch = length $ filter (==ch) s

main :: IO()
main = do
  contents <- readFile "input"
  let (img:_) = lines contents
  print $ snd $ smallestLayer (layers img (25, 6))
