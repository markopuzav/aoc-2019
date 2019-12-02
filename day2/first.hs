import Data.List.Split
import Debug.Trace

getProgram :: String -> [Int]
getProgram str = map read $ splitOn "," str

insertNth :: Int -> Int -> [Int] -> [Int]
insertNth n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs

executeAt :: Int -> [Int] -> [Int]
executeAt n program
    -- | trace ("a = " ++ show a) False = undefined
    | a == 99 = program
    | a == 1 = executeAt (n+4) $ insertNth d (program!!b + program!!c) program
    | a == 2 = executeAt (n+4) $ insertNth d (program!!b * program!!c) program
  where (a:b:c:d:xs) = drop n program

runProgram :: [Int] -> Int
runProgram program = executeAt 0 program !! 0

main = do
  contents <- readFile "input"
  let program = insertNth 1 12 $ insertNth 2 2 $ getProgram contents
  print $ runProgram program
