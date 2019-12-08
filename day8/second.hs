type Image = String
type Layer = String

dim = (25, 6)

layers :: Image -> (Int, Int) -> [Layer]
layers "" _ = []
layers img (w,h) = (take (w*h) img : layers (drop (w*h) img) (w, h))

parseImg :: [Layer] -> (Int, Int) -> [String]
parseImg ls (w, h) = [[pixel i j | j <- [0..w-1]] | i <- [0..h-1]]
  where
    pixel i j = mask $ head $ filter (/= '2') [l !! (w*i + j) | l <- ls]
    mask ch = if ch == '1' then '#' else '.'

main :: IO()
main = do
  contents <- readFile "input"
  let (img:_) = lines contents
  mapM_ print $ parseImg (layers img dim) dim
