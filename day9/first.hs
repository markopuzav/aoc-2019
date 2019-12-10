{-# Language RecordWildCards #-}
import           Data.List
import           Data.List.Split
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Trace

type Tape = Seq Int
type Opcode = (Int, [Int])
type Settings = [Int]
data Computer = Computer {ptr :: Int, tape :: Tape, ins :: [Int], outs :: [Int], relBase :: Int}

getTape :: String -> Tape
getTape str = Seq.fromList $ (map read $ splitOn "," str) ++ replicate 1000000 0

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
 -- | trace ("ptr = " ++ show ptr ++ "|" ++ show op ++ " " ++ show (arg 0) ++ " " ++ show digs ++ "   " ++ show ins ++ " " ++ show (arg 1) ++ " " ++ show (val 1)) False = undefined
 -- | otherwise case op of
      1  -> execute comp{ptr=ptr + 4, tape=set (address 3) (val 1 + val 2) tape, ins=ins,      outs=outs,              relBase=relBase }
      2  -> execute comp{ptr=ptr + 4, tape=set (address 3) (val 1 * val 2) tape, ins=ins,      outs=outs,              relBase=relBase }
      3  -> execute comp{ptr=ptr + 2, tape=set (address 1) (head ins) tape,   ins=tail ins, outs=outs,                 relBase=relBase }
      4  -> execute comp{ptr=ptr + 2, tape=tape,                             ins=ins,      outs=(val 1:outs),      relBase=relBase }
      5  -> execute comp{ptr=if val 1 > 0 then val 2 else ptr+3,  tape=tape, ins=ins,      outs=outs,              relBase=relBase }
      6  -> execute comp{ptr=if val 1 == 0 then val 2 else ptr+3, tape=tape, ins=ins,      outs=outs,              relBase=relBase }
      7  -> execute comp{ptr=ptr + 4, tape=set (address 3) (if val 1 < val 2 then 1 else 0) tape, ins=ins, outs=outs,  relBase=relBase }
      8  -> execute comp{ptr=ptr + 4, tape=set (address 3) (if val 1 == val 2 then 1 else 0) tape, ins=ins, outs=outs, relBase=relBase }
      9  -> execute comp{ptr=ptr + 2, tape=tape, ins=ins, outs=outs,                                       relBase=relBase + val 1 }
      99 -> comp --halt
    where
      address i = if (digs !! (i-1)) == 2 then (tape ! (ptr + i)) + relBase else tape ! (ptr + i)
      arg i = tape ! (ptr + i)
      (op, digs) = parseOp (arg 0)
      val i = [tape ! (arg i), arg i, tape ! (relBase + arg i)] !! (digs !! (i-1))

main :: IO()
main = do
  contents <- readFile "input"
  let tape = getTape contents
  print . outs $ execute Computer{ptr=0, tape=tape, ins=[1], outs=[], relBase=0}