{-# Language RecordWildCards #-}
import           Data.List
import           Data.List.Split
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Tape = Seq Int
type Opcode = (Int, [Int])
type Coor = (Int, Int)
type CoorSet = Set (Int, Int)

data Computer = Computer {ptr :: Int, tape :: Tape, ins :: [Int], outs :: [Int], relBase :: Int}

memSize = 10^5

getTape :: String -> Tape
getTape str = Seq.fromList $ (map read $ splitOn "," str) ++ replicate memSize 0

(!) :: Tape -> Int -> Int
(!) t i
    | i >= length t = undefined
    | i < 0 = undefined
    | otherwise = Seq.index t i

set :: Int -> Int -> Tape -> Tape
set i v m = Seq.update i v m

-- Set value at the index of the Tape
edit :: Int -> Int -> Tape -> Tape
edit index value tape = Seq.update index value tape

parseOp :: Int -> Opcode
parseOp n = (n `mod` 100, [getDigit 2, getDigit 3, getDigit 4])
  where getDigit i = n `div` 10^i `mod` 10

execute :: Computer -> Computer
execute comp@Computer{..} = case op of
      1  -> execute comp{ptr=ptr + 4, tape=set (address 3) (val 1 + val 2) tape}
      2  -> execute comp{ptr=ptr + 4, tape=set (address 3) (val 1 * val 2) tape}
      3  -> execute comp{ptr=ptr + 2, tape=set (address 1) (head ins) tape, ins=tail ins}
      4  -> execute comp{ptr=ptr + 2, outs=(val 1:outs)}
      5  -> execute comp{ptr=if val 1 > 0 then val 2 else ptr+3}
      6  -> execute comp{ptr=if val 1 == 0 then val 2 else ptr+3}
      7  -> execute comp{ptr=ptr + 4, tape=set (address 3) (if val 1 < val 2 then 1 else 0) tape}
      8  -> execute comp{ptr=ptr + 4, tape=set (address 3) (if val 1 == val 2 then 1 else 0) tape}
      9  -> execute comp{ptr=ptr + 2, relBase=relBase + val 1 }
      99 -> comp --halt
    where
      address i = if (digs !! (i-1)) == 2 then (tape ! (ptr + i)) + relBase else tape ! (ptr + i)
      arg i = tape ! (ptr + i)
      (op, digs) = parseOp (arg 0)
      val i = [tape ! (arg i), arg i, tape ! (relBase + arg i)] !! (digs !! (i-1))

countBlockTiles :: [Int] -> Int
countBlockTiles xs = length [i | (i, x) <- zip [0..] xs, i `mod` 3 == 0 && x == 2]

main :: IO()
main = do
  contents <- readFile "input"
  let tape = getTape contents
  print $ countBlockTiles $ outs $ execute Computer{ptr=0, tape=tape, ins=[], outs=[], relBase=0}
