import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.List.Split

type Deck = Seq Int

(!) :: Deck -> Int -> Int
(!) d i = Seq.index d (mod i $ length d)

set :: Int -> Int -> Deck -> Deck
set i v d = Seq.update (mod i $ length d) v d

deck :: Int -> Deck
deck n = Seq.fromList [0..n-1]

dealNew :: Deck -> Deck
dealNew = Seq.reverse

cut :: Deck -> Int -> Deck
cut d i = s Seq.>< f where (f, s) = Seq.splitAt (mod i $ length d) d

dealWith :: Deck -> Int -> Deck
dealWith d k = dealCard 0 (Seq.fromList $ take (length d) [0,0..])
  where
    dealCard i nd
        | i >= length d = nd
        | otherwise     = set (k*i) (d!i) $ dealCard (i+1) nd

applyShuffle :: [String] -> Deck -> Deck
applyShuffle [] d = d
applyShuffle (s:ss) d
    | ws !! 0 == "cut"       = cut rest (read $ last ws)
    | ws !! 2 == "increment" = dealWith rest (read $ last ws)
    | ws !! 2 == "new"       = dealNew rest
  where
    ws = words s
    rest = applyShuffle ss d


main :: IO()
main = do
  contents <- readFile "input"
  let shuffle = lines contents
  let shuffled = applyShuffle (reverse shuffle) (deck 10007)
  print $ Seq.elemIndexL 2019 shuffled
