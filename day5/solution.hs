{-# Language RecordWildCards #-}
import           Data.List.Split
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Tape = Seq Int
type Opcode = (Int, [Int])
data Computer = Computer {ptr :: Int, tape :: Tape, inputs :: [Int], outputs :: [Int]}

iNPUT_VALUE = 5

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
      1  -> execute comp{ptr=ptr + 4, tape=set (arg 3) (val 1 + val 2) tape, inputs=inputs,      outputs=outputs              }
      2  -> execute comp{ptr=ptr + 4, tape=set (arg 3) (val 1 * val 2) tape, inputs=inputs,      outputs=outputs              }
      3  -> execute comp{ptr=ptr + 2, tape=set (arg 1) (head inputs) tape,   inputs=tail inputs, outputs=outputs              }
      4  -> execute comp{ptr=ptr + 2, tape=tape,                             inputs=inputs,      outputs=(val 1:outputs)      }
      5  -> execute comp{ptr=if val 1 > 0 then val 2 else ptr+3,  tape=tape, inputs=inputs,      outputs=outputs              }
      6  -> execute comp{ptr=if val 1 == 0 then val 2 else ptr+3, tape=tape, inputs=inputs,      outputs=outputs              }
      7  -> execute comp{ptr=ptr + 4, tape=set (arg 3) (if val 1 < val 2 then 1 else 0) tape, inputs=inputs, outputs=outputs  }
      8  -> execute comp{ptr=ptr + 4, tape=set (arg 3) (if val 1 == val 2 then 1 else 0) tape, inputs=inputs, outputs=outputs }
      99 -> comp --halt
    where
      arg i = tape ! (ptr + i)
      (op, digs) = parseOp (arg 0)
      val i = [tape ! (arg i), arg i] !! (digs !! (i-1))

main :: IO()
main = do
  contents <- readFile "input"
  let tape = getTape contents
  let test = Computer {ptr=0, tape=tape, inputs=[iNPUT_VALUE], outputs=[]}
  print . head . outputs $ execute test
