{-# Language RecordWildCards #-}
import           Data.List
import           Data.List.Split
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Tape = Seq Int
type Opcode = (Int, [Int])
type Settings = [Int]
data Computer = Computer {ptr :: Int, tape :: Tape, ins :: [Int], outs :: [Int]}

getTape :: String -> Tape
getTape str = Seq.fromList $ map read $ splitOn "," str

(!) :: Tape -> Int -> Int
(!) = Seq.index

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
      1  -> execute comp{ptr=ptr + 4, tape=set (arg 3) (val 1 + val 2) tape, ins=ins,      outs=outs              }
      2  -> execute comp{ptr=ptr + 4, tape=set (arg 3) (val 1 * val 2) tape, ins=ins,      outs=outs              }
      3  -> execute comp{ptr=ptr + 2, tape=set (arg 1) (head ins) tape,   ins=tail ins, outs=outs                 }
      4  -> execute comp{ptr=ptr + 2, tape=tape,                             ins=ins,      outs=(val 1:outs)      }
      5  -> execute comp{ptr=if val 1 > 0 then val 2 else ptr+3,  tape=tape, ins=ins,      outs=outs              }
      6  -> execute comp{ptr=if val 1 == 0 then val 2 else ptr+3, tape=tape, ins=ins,      outs=outs              }
      7  -> execute comp{ptr=ptr + 4, tape=set (arg 3) (if val 1 < val 2 then 1 else 0) tape, ins=ins, outs=outs  }
      8  -> execute comp{ptr=ptr + 4, tape=set (arg 3) (if val 1 == val 2 then 1 else 0) tape, ins=ins, outs=outs }
      99 -> comp --halt
    where
      arg i = tape ! (ptr + i)
      (op, digs) = parseOp (arg 0)
      val i = [tape ! (arg i), arg i] !! (digs !! (i-1))

getThrust :: Settings -> Tape -> Int
getThrust settings tape = foldl plug 0 settings
  where plug lastOut s = head . outs $ execute Computer {ptr=0, tape=tape, ins=[s, lastOut], outs=[]}

getMaxThrust :: Tape -> Int
getMaxThrust tape = maximum [getThrust s tape | s <- permutations [0..4]]

main :: IO()
main = do
  contents <- readFile "input"
  let tape = getTape contents
  print $ getMaxThrust tape
