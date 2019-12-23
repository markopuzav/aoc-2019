import Debug.Trace

inv :: Integer -> Integer -> Maybe Integer
inv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

getTransform :: [String] -> Integer -> (Integer, Integer)
getTransform [] _ = (1, 0)
getTransform (s:ss) n
    -- | trace ("a, b " ++ show (a,b)) False = undefined
    | ws !! 0 == "cut"       = md (a, b - (read $ last ws))
    | ws !! 2 == "increment" = md ((read $ last ws) * a, (read $ last ws) * b)
    | ws !! 2 == "new"       = md (-a, n - 1 - b)
  where
    ws = words s
    (a, b) = getTransform ss n
    md (x, y) = (mod x n, mod y n)

expTransform :: (Integer, Integer) -> Int -> Integer -> (Integer, Integer)
expTransform (a, b) 0 m = (1, 0)
expTransform (a, b) n m
    | mod n 2 == 1 = let (aa, bb) = expTransform (a, b) (n - 1) m   in md (a*aa, b + a*bb)
    | mod n 2 == 0 = let (aa, bb) = expTransform (a, b) (div n 2) m in md (aa*aa, bb + aa*bb)
  where md (x, y) = (mod x m, mod y m)

main :: IO()
main = do
  contents <- readFile "input"
  let (cards, repeats) = (119315717514047, 101741582076661)
  let shuffle = reverse $ lines contents
  let (a, b) = expTransform (getTransform shuffle cards) repeats cards
  print $ let Just aInv = inv a cards in mod (aInv * (2020 - b)) cards
